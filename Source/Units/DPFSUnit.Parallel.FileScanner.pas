unit DPFSUnit.Parallel.FileScanner;

interface

{TODO: Could we move more code to the to the TParallelFileScannerCustom? Have to think about it.
       Still way too much duplicate code between "reguar" and Spring4D version }

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.IOUtils, System.Math, System.SyncObjs, System.SysUtils, System.Generics.Collections
  {$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl, OtlContainers
  {$ENDIF}
  ;

type
  TDirectoryWalkProc = reference to procedure(const AFileName: string);

  TFileScanExclusions = record
  strict private
    FUpdateCount: Integer;
    FPathPrefixes: TArray<string>;
    FPathSuffixes: TArray<string>;
    FUpperPathPrefixes: TArray<string>;
    FUpperPathSuffixes: TArray<string>;
    function GetPathPrefixesString: string;
    function GetPathSuffixesString: string;
    function GetUpperPathPrefixes: TArray<string>;
    function GetUpperPathSuffixes: TArray<string>;
    procedure CacheUpperPrefixes;
    procedure SetPathPrefixes(const AValue: TArray<string>);
    procedure SetPathSuffixes(const AValue: TArray<string>);
  public
    class operator Initialize(out ADest: TFileScanExclusions);
    procedure BeginUpdate;
    procedure EndUpdate;
    property PathPrefixes: TArray<string> read FPathPrefixes write SetPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write SetPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
  public
    property UpperPathPrefixes: TArray<string> read GetUpperPathPrefixes;
    property UpperPathSuffixes: TArray<string> read GetUpperPathSuffixes;
  end;

  TParallelFileScannerCustom = class(TObject)
  strict private
    FSkippedDirectories: TStringList;
    FCachedSkippedDirectoriesFileCount: Integer;
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
    FLock: TCriticalSection;
    FSkippedFilesCount: Integer;
    FSortResultList: Boolean;
    function ExcludedFilenameByuSuffix(const AFileName: string): Boolean;
    function ExcludedPathByPrefix(const APath: string): Boolean;
    procedure CheckGetFilesParameters(const APath: string; const ASearchPattern: string);
    procedure GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const AFiles: TStringList); virtual;
    procedure InternalCheckDirPathParam(const APath: string; const AExistsCheck: Boolean);
    procedure MergeResultLists(const AResultList: TObjectList<TStringList>; const AResult: TStringList); virtual;
    procedure WalkThroughDirectory(const APath, APattern: string; const APreCallback: TDirectoryWalkProc; const ARecursive: Boolean);
    //
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
      const AFileNamesList: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean; overload; virtual;
    {$ENDIF}
  public
    constructor Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True); overload;
    constructor Create(const AExtensions: TStringList; const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    property SkippedFilesCount: Integer read GetSkippedFilesCount write FSkippedFilesCount;
    property SortResultList: Boolean read FSortResultList write FSortResultList;
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

implementation

uses
  System.Diagnostics
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, GpStuff
{$ELSE}
  , System.Threading
{$ENDIF};

{ TFileScanExcludes }

procedure TFileScanExclusions.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFileScanExclusions.CacheUpperPrefixes;
begin
  SetLength(FUpperPathPrefixes, Length(FPathPrefixes));

  for var LIndex := Low(FPathPrefixes) to High(FPathPrefixes) do
    FUpperPathPrefixes[LIndex] := ExcludeTrailingPathDelimiter(FPathPrefixes[LIndex].ToUpper);

  SetLength(FUpperPathSuffixes, Length(FPathSuffixes));

  for var LIndex := Low(FPathSuffixes) to High(FPathSuffixes) do
    FUpperPathSuffixes[LIndex] := ExcludeTrailingPathDelimiter(FPathSuffixes[LIndex].ToUpper);
end;

procedure TFileScanExclusions.EndUpdate;
begin
  FUpdateCount := Max(0, FUpdateCount - 1);

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

function TFileScanExclusions.GetPathPrefixesString: string;
begin
  Result := Result.Join(';', FPathPrefixes);
end;

function TFileScanExclusions.GetPathSuffixesString: string;
begin
  Result := Result.Join(';', FPathSuffixes);
end;

function TFileScanExclusions.GetUpperPathPrefixes: TArray<string>;
begin
  Result := FUpperPathPrefixes;
end;

function TFileScanExclusions.GetUpperPathSuffixes: TArray<string>;
begin
  Result := FUpperPathSuffixes;
end;

class operator TFileScanExclusions.Initialize(out ADest: TFileScanExclusions);
begin
  ADest.FUpdateCount := 0;
end;

procedure TFileScanExclusions.SetPathPrefixes(const AValue: TArray<string>);
begin
  FPathPrefixes := AValue;

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

procedure TFileScanExclusions.SetPathSuffixes(const AValue: TArray<string>);
begin
  FPathSuffixes := AValue;

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

{ TParallelFileScannerCustom }

procedure TParallelFileScannerCustom.AddSkippedDirectories(const APath: string);
begin
  for var LIndex := 0 to FSkippedDirectories.Count - 1 do
  begin
    if APath.StartsWith(FSkippedDirectories[LIndex]) then
      Exit;
  end;

  if FSkippedDirectories.IndexOf(APath) = -1 then
    FSkippedDirectories.Add(APath);
end;

procedure TParallelFileScannerCustom.CheckGetFilesParameters(const APath: string; const ASearchPattern: string);
var
  LPath: string;
begin
  LPath := TPath.GetFullPath(APath);

  if Trim(ASearchPattern) = '' then
    raise EInOutArgumentException.Create('Empty Search pattern')
  else if not TPath.HasValidFileNameChars(ASearchPattern, True) then
    raise EInOutArgumentException.Create('Search pattern Has invalid characters');

  InternalCheckDirPathParam(LPath, True);
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True);
begin
  inherited Create;

  FCachedSkippedDirectoriesFileCount := 0;
  FSkippedDirectories := TStringList.Create;
  FSkippedDirectories.Sorted := True;
  FSortResultList := ASortResultList;
  FLock := TCriticalSection.Create;
  FExtensions := TStringList.Create;
  FExtensions.AddStrings(AExtensions);
end;

destructor TParallelFileScannerCustom.Destroy;
begin
  FSkippedDirectories.Free;
  FExtensions.Free;
  FLock.Free;

  inherited Destroy;
end;

function TParallelFileScannerCustom.ExcludedPathByPrefix(const APath: string): Boolean;
var
  LPath: string;
begin
  Result := False;

  LPath := ExcludeTrailingPathDelimiter(APath.ToUpper);

  for var LIndex := Low(FExclusions.UpperPathPrefixes) to High(FExclusions.UpperPathPrefixes) do
  begin
    var LUpperPrefix := FExclusions.UpperPathPrefixes[LIndex];

    if LPath.StartsWith(LUpperPrefix) then
      Exit(True);
  end;
end;

function TParallelFileScannerCustom.ExcludedFilenameByuSuffix(const AFileName: string): Boolean;
var
  LCurrentFilename: string;
  LCurrentExcludedFilename: string;
begin
  Result := False;

  LCurrentFilename := AFileName.ToUpper;

  for var LIndex := Low(FExclusions.UpperPathSuffixes) to High(FExclusions.UpperPathSuffixes) do
  begin
    LCurrentExcludedFilename := FExclusions.UpperPathSuffixes[LIndex];

    if LCurrentFilename.EndsWith(LCurrentExcludedFilename) then
      Exit(True)
  end;
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

function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
var
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  LTaskConfig: IOmniTaskConfig;
{$ENDIF}
  LCurrentRootPath: string;
  LListsOfFilelists: TObjectList<TStringList>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;
  FExclusions := AExclusions;

  FSkippedFilesCount := 0;

  LListsOfFilelists := TObjectList<TStringList>.Create(True);
  try
    for LCurrentRootPath in ADirectories do
    begin
      if not DirectoryExists(LCurrentRootPath) then
        Continue;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
      LTaskConfig := Parallel.TaskConfig;
      LTaskConfig.SetPriority(APriority);

      Parallel
        .&for(0, FExtensions.Count - 1)
        .TaskConfig(LTaskConfig)
        .NumTasks(TThread.ProcessorCount)
        .Execute(
{$ELSE}
      TParallel
        .&for(0, FExtensions.Count - 1,
{$ENDIF}
        procedure(AIndex: Integer)
        var
          LExtension: string;
          LTempFileNames: TStringList;
        begin
          LExtension := FExtensions[AIndex];
          LTempFileNames := TStringList.Create;

          // Seems that this GetFiles ripped from the RTL, ported to use Spring container. MAYBE it is not faster after all.
          // Should make two version, one using standard RTL and one using this. Time them, which is faster.
          // This is now about 650ms so maybe no use to optimize much
          GetFiles(LCurrentRootPath, LExtension, TSearchOption.soAllDirectories, LTempFileNames);

          if LTempFileNames.Count > 0 then
          begin
            FLock.Acquire;
            try
              LListsOfFilelists.Add(LTempFileNames);
            finally
              FLock.Release;
            end;
          end
          else
            LTempFileNames.Free;
        end
      );
    end;

    FLock.Acquire;
    try
      MergeResultLists(LListsOfFilelists, AFileNamesList);

      if FSortResultList then
        AFileNamesList.Sort;
    finally
      FLock.Release;
    end;
  finally
    LListsOfFilelists.Free;
  end;

  Result := AFileNamesList.Count > 0;
  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
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
  const AFileNamesList: IOmniValueQueue; var AFileCount: Integer; const APriority: TOTLThreadPriority = tpNormal): Boolean;
var
  LTaskConfig: IOmniTaskConfig;
  LCurrentRootPath: string;
  LFileScanStopWatch: TStopwatch;
  LOmniValue: TOmniValue;
  LFileCount: TGp4AlignedInt;
begin
  LFileScanStopWatch := TStopwatch.StartNew;
  FExclusions := AExclusions;

  FSkippedFilesCount := 0;
  AFileCount := 0;
  LFileCount.Value := 0;

  for LCurrentRootPath in ADirectories do
  begin
    if not DirectoryExists(LCurrentRootPath) then
      Continue;

    LTaskConfig := Parallel.TaskConfig;
    LTaskConfig.SetPriority(APriority);

    Parallel
      .&for(0, FExtensions.Count - 1)
      .TaskConfig(LTaskConfig)
      .NumTasks(TThread.ProcessorCount)
      .Execute(
      procedure(AIndex: Integer)
      var
        LExtension: string;
        LTempFileNames: TStringList;
        LIndex: Integer;
      begin
        LExtension := FExtensions[AIndex];
        LTempFileNames := TStringList.Create;
        try
          GetFiles(LCurrentRootPath, LExtension, TSearchOption.soAllDirectories, LTempFileNames);

          for LIndex := 0 to LTempFileNames.Count - 1 do
          begin
            LOmniValue := LTempFileNames[LIndex];

            AFileNamesList.Enqueue(LOmniValue);
            LFileCount.Increment;
          end;
        finally
          LTempFileNames.Free;
        end;
      end
    );
  end;

  AFileCount := LFileCount.Value;
  Result := AFileCount > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;
{$ENDIF}


procedure TParallelFileScannerCustom.GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption;
  const AFiles: TStringList);
var
  LPreCallback: TDirectoryWalkProc;
begin
  CheckGetFilesParameters(APath, ASearchPattern);

  AFiles.Capacity := 1024 * 16;

  LPreCallback :=
    procedure (const AFilename: string)
    begin
      AFiles.Add(AFilename);
    end;

  WalkThroughDirectory(APath, ASearchPattern, LPreCallback, ASearchOption = TSearchOption.soAllDirectories);
end;

function TParallelFileScannerCustom.GetSkippedFilesCount: Integer;
begin
  if (FSkippedDirectories.Count > 0) and (FCachedSkippedDirectoriesFileCount = 0) then
    FCachedSkippedDirectoriesFileCount := GetFileCounts(FSkippedDirectories);

  Result := FSkippedFilesCount + FCachedSkippedDirectoriesFileCount;
end;

procedure TParallelFileScannerCustom.InternalCheckDirPathParam(const APath: string; const AExistsCheck: Boolean);
begin
  if Trim(APath) = '' then
    raise EInOutArgumentException.Create('Emptyy path')
  else if not TPath.HasValidPathChars(APath, False) then
    raise EInOutArgumentException.Create('Path has invalid characters')
  else if AExistsCheck and (not TDirectory.Exists(APath)) then
    raise EDirectoryNotFoundException.Create('Path does not exist');
end;

procedure TParallelFileScannerCustom.MergeResultLists(const AResultList: TObjectList<TStringList>;
  const AResult: TStringList);
var
  I: Integer;
  J: Integer;
  LFileName: string;
  LUniqueFiles: TDictionary<string, Integer>;
begin
  LUniqueFiles := TDictionary<string, Integer>.Create;
  try
    LUniqueFiles.Capacity := 2000;

    for I := 0 to AResultList.Count - 1 do
    begin
      for J := 0 to AResultList[I].Count - 1 do
      begin
        LFileName := AResultList[I][J];

        if not LUniqueFiles.ContainsKey(LFileName) then
          LUniqueFiles.Add(LFileName, 0);
      end;
    end;

    AResult.AddStrings(LUniqueFiles.Keys.ToArray);
  finally
    LUniqueFiles.Free;
  end;
end;

procedure TParallelFileScannerCustom.WalkThroughDirectory(const APath, APattern: string;
  const APreCallback: TDirectoryWalkProc; const ARecursive: Boolean);
var
  LSearchRec: TSearchRec;
  LIsDirectory: Boolean;
begin
  if FindFirst(TPath.Combine(APath, '*', False), faAnyFile, LSearchRec) = 0 then
  try
    repeat
      if LSearchRec.Name = CURRENT_DIR then
        Continue;

      if LSearchRec.Name = PARENT_DIR then
        Continue;

      LIsDirectory := LSearchRec.Attr and System.SysUtils.faDirectory <> 0;

      if not LIsDirectory then
      begin
        if TPath.MatchesPattern(LSearchRec.Name, APattern, False) then
        begin
          // call the pre-order callback method
          var LFilename := TPath.Combine(APath, LSearchRec.Name, False); // Weirdly "APath + PathDelim..." is slower than TPath.Combine

          if not ExcludedFilenameByuSuffix(LFilename) then
          begin
            Inc(FSkippedFilesCount);

            APreCallback(LFilename);
          end;
        end;
      end;

      // go recursive in subdirectories
      if ARecursive and LIsDirectory then
      begin
        var LNewPath :=  TPath.Combine(APath, LSearchRec.Name, False);

        if not ExcludedPathByPrefix(LNewPath) then
          WalkThroughDirectory(LNewPath, APattern, APreCallback, ARecursive)
        else
          AddSkippedDirectories(LNewPath);
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
