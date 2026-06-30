unit DPFSUnit.Parallel.FileScanner;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.Generics.Collections, System.IOUtils, System.SyncObjs, System.SysUtils
  {$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl, OtlContainers
  {$ENDIF};

type
  TDirectoryWalkProc = reference to procedure(const AFileName: string);

  TExclusionKind = (ekPathPrefixes, ekPathSuffixes);

  // A single unit of parallel scanning work: one directory, walked recursively or not.
  // A root is split into one non-recursive job for its own files plus one recursive job per
  // immediate subdirectory, so even a single root can be scanned by several worker threads.
  TScanJob = record
    Directory: string;
    Recursive: Boolean;
  end;

  TFileScanExclusions = record
  strict private
    FPathPrefixes: TArray<string>;
    FPathSuffixes: TArray<string>;
    function GetPathPrefixesString: string;
    function GetPathSuffixesString: string;
  public
    procedure InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
    property PathPrefixes: TArray<string> read FPathPrefixes write FPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write FPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
  end;

  TParallelFileScannerCustom = class(TObject)
  strict private
    FSkippedDirectories: TStringList;
    FCachedSkippedDirectoriesFileCount: Integer;
    FConvertRelativePathsToAbsolute: Boolean;
    FFastExtensions: TArray<string>;  // extensions (with dot, e.g. '.pas') for simple "*.ext" patterns
    FComplexPatterns: TArray<string>; // patterns that still need full TPath.MatchesPattern
    function GetThreadCount(const ATaskCount: Integer): Integer;
    function GetSkippedFilesCount: Integer;
    function GetFileCounts(const ASkippedDirectories: TStringList): Integer;
    procedure AddSkippedDirectories(const APath: string);
  strict protected
    const
      CURRENT_DIR: string = '.';
      PARENT_DIR: string = '..';
  strict protected
    FDiskScanTimeForFiles: Double;
    FExclusions: TFileScanExclusions;
    FExtensions: TStringList;
    FLock: TMonitor;
    FSkippedFilesCount: Integer;
    FSortResultList: Boolean;
    procedure ResetCounters;
    procedure PrepareExtensions;
    function ExcludedFileNameBySuffix(const AFileName: string): Boolean; inline;
    function ExcludedPathByPrefix(const APath: string): Boolean; inline;
    function MatchesAnyExtension(const AFileName: string): Boolean; inline;
    function BuildScanJobs(const ADirectories: TArray<string>): TArray<TScanJob>;
    procedure WalkThroughDirectory(const APath: string; const APreCallback: TDirectoryWalkProc; const ARecursive: Boolean);
    // Shared scan/merge core for both the RTL and Spring4D result containers; fills AResult.
    procedure ScanInto(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const AResult: TStringList
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
    {$ENDIF});
    // GetFileList overloads
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const AFileNamesList: TStringList
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
    {$ENDIF}): Boolean; overload; virtual;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions; const AFileNamesList: TStringList
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
    {$ENDIF}): Boolean; overload; virtual;
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesOmniValueQueue: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean; overload; virtual;
    {$ENDIF}
  public
    constructor Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True); overload;
    constructor Create(const AExtensions: TStringList; const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    property SkippedFilesCount: Integer read GetSkippedFilesCount;
    property SortResultList: Boolean read FSortResultList write FSortResultList;
    property ConvertRelativePathsToAbsolute: Boolean read FConvertRelativePathsToAbsolute write FConvertRelativePathsToAbsolute;
  end;

  TParallelFileScanner = class(TParallelFileScannerCustom)
  public
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions; const AFileNamesList: TStringList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; overload; override;
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const AFileNamesList: TStringList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; overload; override;
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesList: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean; override;
    {$ENDIF}
  end;

  procedure InitArrayDataFromStrings(var AArray: TArray<string>; const AArrayData: TStrings);

implementation

uses
  System.Diagnostics, System.Math
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, GpStuff
{$ELSE}
  , System.Threading
{$ENDIF};

procedure InitArrayDataFromStrings(var AArray: TArray<string>; const AArrayData: TStrings);
var
  LIndex: Integer;
begin
  AArray := [];
  SetLength(AArray, AArrayData.Count);

  for LIndex := 0 to AArrayData.Count - 1 do
    AArray[LIndex] := AArrayData[LIndex];
end;

{ TFileScanExclusions }

function TFileScanExclusions.GetPathPrefixesString: string;
begin
  Result := Result.Join(';', FPathPrefixes);
end;

function TFileScanExclusions.GetPathSuffixesString: string;
begin
  Result := Result.Join(';', FPathSuffixes);
end;

procedure TFileScanExclusions.InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
begin
  case AKind of
    ekPathPrefixes: InitArrayDataFromStrings(FPathPrefixes, AArrayData);
    ekPathSuffixes: InitArrayDataFromStrings(FPathSuffixes, AArrayData);
  end;
end;

{ TParallelFileScannerCustom }

procedure TParallelFileScannerCustom.AddSkippedDirectories(const APath: string);
begin
  // Called from WalkThroughDirectory, which runs on multiple worker threads, so
  // access to the shared FSkippedDirectories list must be serialized.
  FLock.Enter(FSkippedDirectories);
  try
    for var LIndex := 0 to FSkippedDirectories.Count - 1 do
    begin
      if APath.StartsWith(FSkippedDirectories[LIndex]) then
        Exit;
    end;

    if FSkippedDirectories.IndexOf(APath) = -1 then
      FSkippedDirectories.Add(APath);
  finally
    FLock.Exit(FSkippedDirectories);
  end;
end;

function TParallelFileScannerCustom.BuildScanJobs(const ADirectories: TArray<string>): TArray<TScanJob>;

  procedure AddJob(const AJobs: TList<TScanJob>; const ADirectory: string; const ARecursive: Boolean);
  var
    LJob: TScanJob;
  begin
    LJob.Directory := ADirectory;
    LJob.Recursive := ARecursive;

    AJobs.Add(LJob);
  end;

var
  LJobs: TList<TScanJob>;
  LSearchRec: TSearchRec;
  LRoot: string;
begin
  LJobs := TList<TScanJob>.Create;
  try
    for var LRootPath in ADirectories do
    begin
      if not TDirectory.Exists(LRootPath) then
        Continue;

      LRoot := LRootPath;

      // Resolve the root to an absolute path once, so every file produced from it is already
      // absolute and no per-file conversion pass is needed afterwards.
      if FConvertRelativePathsToAbsolute then
        LRoot := TPath.GetFullPath(LRoot);

      // The root itself is scanned non-recursively for its own files; each immediate
      // subdirectory becomes a recursive job of its own so a single root still parallelizes.
      AddJob(LJobs, LRoot, False);

      if FindFirst(TPath.Combine(LRoot, '*', False), faAnyFile, LSearchRec) = 0 then
      try
        repeat
          if LSearchRec.Attr and System.SysUtils.faDirectory = 0 then
            Continue; // only interested in subdirectories here

          if (LSearchRec.Name = CURRENT_DIR) or (LSearchRec.Name = PARENT_DIR) then
            Continue;

          var LSubDirectory := TPath.Combine(LRoot, LSearchRec.Name, False);

          if ExcludedPathByPrefix(LSubDirectory) then
            AddSkippedDirectories(LSubDirectory)
          else
            AddJob(LJobs, LSubDirectory, True);
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
    end;

    Result := LJobs.ToArray;
  finally
    LJobs.Free;
  end;
end;

procedure TParallelFileScannerCustom.PrepareExtensions;
var
  LFast: TList<string>;
  LComplex: TList<string>;
begin
  LFast := TList<string>.Create;
  LComplex := TList<string>.Create;
  try
    for var LIndex := 0 to FExtensions.Count - 1 do
    begin
      var LPattern := FExtensions[LIndex];

      if Trim(LPattern) = '' then
        raise EInOutArgumentException.Create('Empty search pattern')
      else if not TPath.HasValidFileNameChars(LPattern, True) then
        raise EInOutArgumentException.Create('Search pattern has invalid characters');

      // A plain "*.ext" pattern (no further wildcards) can be matched with a fast, allocation-free
      // extension compare instead of TPath.MatchesPattern, which would run once per file scanned.
      if LPattern.StartsWith('*.') then
      begin
        var LExtension := LPattern.Substring(1); // ".ext" from "*.ext"

        if (Length(LExtension) > 1) and (LExtension.IndexOfAny(['*', '?']) < 0) then
          LFast.Add(LExtension)
        else
          LComplex.Add(LPattern);
      end
      else
        LComplex.Add(LPattern);
    end;

    FFastExtensions := LFast.ToArray;
    FComplexPatterns := LComplex.ToArray;
  finally
    LComplex.Free;
    LFast.Free;
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True);
begin
  inherited Create;

  FCachedSkippedDirectoriesFileCount := 0;
  FSkippedDirectories := TStringList.Create;
  FSkippedDirectories.Sorted := True;
  FSortResultList := ASortResultList;
  FExtensions := TStringList.Create;
  FExtensions.AddStrings(AExtensions);
end;

destructor TParallelFileScannerCustom.Destroy;
begin
  FSkippedDirectories.Free;
  FExtensions.Free;

  inherited Destroy;
end;

procedure TParallelFileScannerCustom.ResetCounters;
begin
  FSkippedFilesCount := 0;
  FSkippedDirectories.Clear;
  FCachedSkippedDirectoriesFileCount := 0;
end;

function TParallelFileScannerCustom.ExcludedPathByPrefix(const APath: string): Boolean;
begin
  Result := False;

  if Length(FExclusions.PathPrefixes) = 0 then
    Exit; // no prefixes configured: skip the work entirely (called once per subdirectory)

  var LPath := ExcludeTrailingPathDelimiter(APath);

  for var LPrefix in FExclusions.PathPrefixes do
    if LPath.StartsWith(ExcludeTrailingPathDelimiter(LPrefix), True) then // True => case-insensitive
      Exit(True);
end;

function TParallelFileScannerCustom.ExcludedFileNameBySuffix(const AFileName: string): Boolean;
begin
  Result := False;

  for var LSuffix in FExclusions.PathSuffixes do
    if AFileName.EndsWith(LSuffix, True) then // True => case-insensitive
      Exit(True);
end;

function TParallelFileScannerCustom.MatchesAnyExtension(const AFileName: string): Boolean;
begin
  Result := False;

  // Fast path: a plain "*.ext" pattern is just a case-insensitive suffix test (no allocation).
  for var LFast in FFastExtensions do
    if AFileName.EndsWith(LFast, True) then
      Exit(True);

  for var LPattern in FComplexPatterns do
    if TPath.MatchesPattern(AFileName, LPattern, False) then
      Exit(True);
end;

function TParallelFileScannerCustom.GetFileCounts(const ASkippedDirectories: TStringList): Integer;
var
  LCurrentDir: string;
begin
  Result := 0;

  for var LDirectoryIndex := 0 to ASkippedDirectories.Count - 1 do
  begin
    LCurrentDir := ASkippedDirectories[LDirectoryIndex];

    if not LCurrentDir.Trim.IsEmpty then
      for var LExtensionIndex := 0 to FExtensions.Count - 1 do
        Inc(Result, Length(TDirectory.GetFiles(LCurrentDir, FExtensions[LExtensionIndex], TSearchOption.soAllDirectories)));
  end;
end;

procedure TParallelFileScannerCustom.ScanInto(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AResult: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF});
const
  MERGE_INITIAL_CAPACITY = 2000;
var
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  LTaskConfig: IOmniTaskConfig;
{$ENDIF}
  LListOfFileLists: TObjectList<TStringList>;
  LScanJobs: TArray<TScanJob>;
  LFileScanStopWatch: TStopwatch;
  LUniqueFiles: TDictionary<string, Boolean>;
  LUpperBound: Integer;
begin
  LFileScanStopWatch := TStopwatch.StartNew;
  FExclusions := AExclusions;

  ResetCounters;
  PrepareExtensions;

  LScanJobs := BuildScanJobs(ADirectories);

  LListOfFileLists := TObjectList<TStringList>.Create(True);
  LUniqueFiles := TDictionary<string, Boolean>.Create(MERGE_INITIAL_CAPACITY);
  try
    if Length(LScanJobs) > 0 then
{$IFDEF USE_OMNI_THREAD_LIBRARY}
    begin
      LTaskConfig := Parallel.TaskConfig;
      LTaskConfig.SetPriority(APriority);

      Parallel
        .&for(0, High(LScanJobs))
        .TaskConfig(LTaskConfig)
        .NumTasks(GetThreadCount(Length(LScanJobs)))
        .Execute(
{$ELSE}
      TParallel
        .&for(0, High(LScanJobs),
{$ENDIF}
        procedure(AIndex: Integer)
        var
          LTempFileNames: TStringList;
        begin
          LTempFileNames := TStringList.Create;
          try
            WalkThroughDirectory(LScanJobs[AIndex].Directory,
              procedure(const AFileName: string)
              begin
                LTempFileNames.Add(AFileName);
              end,
              LScanJobs[AIndex].Recursive);

            if LTempFileNames.Count > 0 then
            begin
              FLock.Enter(LListOfFileLists);
              try
                LListOfFileLists.Add(LTempFileNames);
                LTempFileNames := nil; // ownership transferred to the list
              finally
                FLock.Exit(LListOfFileLists);
              end;
            end;
          finally
            LTempFileNames.Free; // no-op if ownership was transferred above
          end;
        end
      );
{$IFDEF USE_OMNI_THREAD_LIBRARY}
    end;
{$ENDIF}

    // All worker tasks have finished here (Execute blocks), so no lock is needed.
    // Deduplicate across the per-job lists (only matters when scanned roots overlap),
    // preserving first-seen order; sorting is applied only when SortResultList is set.
    LUpperBound := 0;

    for var LList in LListOfFileLists do
      Inc(LUpperBound, LList.Count);

    AResult.Capacity := AResult.Count + LUpperBound;

    for var LList in LListOfFileLists do
      for var LFileName in LList do
        if LUniqueFiles.TryAdd(LFileName, True) then
          AResult.Add(LFileName);

    if FSortResultList then
      AResult.Sort;
  finally
    LUniqueFiles.Free;
    LListOfFileLists.Free;
  end;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  ScanInto(ADirectories, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
    , APriority
{$ENDIF});

  Result := AFileNamesList.Count > 0;
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , APriority
{$ENDIF});
end;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesOmniValueQueue: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean;
var
  LTaskConfig: IOmniTaskConfig;
  LScanJobs: TArray<TScanJob>;
  LFileScanStopWatch: TStopwatch;
  LFileCount: TGp4AlignedInt;
begin
  LFileScanStopWatch := TStopwatch.StartNew;
  FExclusions := AExclusions;

  ResetCounters;
  PrepareExtensions;
  AFileCount := 0;
  LFileCount.Value := 0;

  LScanJobs := BuildScanJobs(ADirectories);

  if Length(LScanJobs) > 0 then
  begin
    LTaskConfig := Parallel.TaskConfig;
    LTaskConfig.SetPriority(APriority);

    Parallel
      .&for(0, High(LScanJobs))
      .TaskConfig(LTaskConfig)
      .NumTasks(GetThreadCount(Length(LScanJobs)))
      .Execute(
      procedure(AIndex: Integer)
      begin
        WalkThroughDirectory(LScanJobs[AIndex].Directory,
          procedure(const AFileName: string)
          var
            LOmniValue: TOmniValue;
          begin
            // IOmniValueQueue is thread-safe and LOmniValue is local to this callback,
            // so no external lock is needed around the enqueue.
            LOmniValue.AsString := AFileName;
            AFileNamesOmniValueQueue.Enqueue(LOmniValue);
            LFileCount.Increment;
          end,
          LScanJobs[AIndex].Recursive);
      end
    );
  end;

  AFileCount := LFileCount.Value;
  Result := AFileCount > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;
{$ENDIF}

function TParallelFileScannerCustom.GetThreadCount(const ATaskCount: Integer): Integer;
begin
  Result := Min(ATaskCount, TTHread.ProcessorCount);
end;

function TParallelFileScannerCustom.GetSkippedFilesCount: Integer;
begin
  if (FSkippedDirectories.Count > 0) and (FCachedSkippedDirectoriesFileCount = 0) then
    FCachedSkippedDirectoriesFileCount := GetFileCounts(FSkippedDirectories);

  Result := FSkippedFilesCount + FCachedSkippedDirectoriesFileCount;
end;

procedure TParallelFileScannerCustom.WalkThroughDirectory(const APath: string; const APreCallback: TDirectoryWalkProc;
  const ARecursive: Boolean);
var
  LSearchRec: TSearchRec;
  LIsDirectory: Boolean;
  LDirPrefix: string;
begin
  // Build "APath\" once so each entry below is a single concatenation rather than a
  // TPath.Combine call (which re-checks the trailing delimiter for every entry).
  LDirPrefix := IncludeTrailingPathDelimiter(APath);

  if FindFirst(LDirPrefix + '*', faAnyFile, LSearchRec) = 0 then
  try
    repeat
      if (LSearchRec.Name = CURRENT_DIR) or (LSearchRec.Name = PARENT_DIR) then
        Continue;

      LIsDirectory := LSearchRec.Attr and System.SysUtils.faDirectory <> 0;

      if LIsDirectory then
      begin
        // go recursive into subdirectories
        if ARecursive then
        begin
          var LNewPath := LDirPrefix + LSearchRec.Name;

          if ExcludedPathByPrefix(LNewPath) then
            AddSkippedDirectories(LNewPath)
          else
            WalkThroughDirectory(LNewPath, APreCallback, ARecursive);
        end;
      end
      else if MatchesAnyExtension(LSearchRec.Name) then
      begin
        var LFilename := LDirPrefix + LSearchRec.Name;

        // Runs on multiple worker threads, so the skipped counter must be incremented atomically.
        if ExcludedFileNameBySuffix(LFilename) then
          AtomicIncrement(FSkippedFilesCount)
        else
          APreCallback(LFilename);
      end;
    until FindNext(LSearchRec) <> 0;
  finally
    FindClose(LSearchRec);
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TStringList; const ASortResultList: Boolean = True);
begin
  Create(AExtensions.ToStringArray, ASortResultList);
end;

{ TParallelFileScanner }

function TParallelFileScanner.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  Result := inherited GetFileList(ADirectories, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , APriority
{$ENDIF});
end;

function TParallelFileScanner.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  Result := inherited GetFileList(ADirectories, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , APriority
{$ENDIF});
end;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
function TParallelFileScanner.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean;
begin
  Result := inherited GetFileList(ADirectories, AExclusions, AFileNamesList, AFileCount, APriority);
end;
{$ENDIF}


end.
