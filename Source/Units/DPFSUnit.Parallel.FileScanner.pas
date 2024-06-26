unit DPFSUnit.Parallel.FileScanner;

interface

{TODO: Could we move more code to the to the TParallelFileScannerCustom? Have to think about it.
       Still way too much duplicate code between "reguar" and spring version }

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.IOUtils, System.Math, System.SyncObjs, System.SysUtils, System.Generics.Collections;

type
  TDirectoryWalkProc = reference to function (const APath: string; const AFileInfo: TSearchRec): Boolean;

  TFileScanExcludes = record
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
    procedure SetPathPrefixes(const Value: TArray<string>);
    procedure SetPathSuffixes(const Value: TArray<string>);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property PathPrefixes: TArray<string> read FPathPrefixes write SetPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write SetPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
  private
  public
    property UpperPathPrefixes: TArray<string> read GetUpperPathPrefixes;
    property UpperPathSuffixes: TArray<string> read GetUpperPathSuffixes;
  end;

  TParallelFileScannerCustom = class(TObject)
  strict protected
    const
      CURRENT_DIR: string = '.';
      PARENT_DIR: string = '..';
  strict protected
    FDiskScanTimeForFiles: Double;
    FExclusions: TFileScanExcludes;
    FExtensions: TStringList;
    FLock: TCriticalSection;
    FSkippedFilesCount: Integer;
    FSortResultList: Boolean;
    procedure InternalCheckDirPathParam(const APath: string; const AExistsCheck: Boolean);
    procedure CheckGetFilesParameters(const APath: string; const ASearchPattern: string);
    procedure GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const AFiles: TStringList); virtual;
    function ExcludedPathByPrefix(const APath: string): Boolean;
    function ExcludedFilenameByuSuffix(const AFileName: string): Boolean;
    procedure WalkThroughDirectory(const APath, APattern: string; const APreCallback: TDirectoryWalkProc; const ARecursive: Boolean);
    procedure MergeResultLists(const AResultList: TObjectList<TStringList>; const AResult: TStringList); virtual;
    //
    function GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes; var AFileNamesList: TStringList): Boolean; overload; virtual;
    function GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes; var AFileNamesList: TStringList): Boolean; overload; virtual;
  public
    constructor Create(const AExtensions: TStringList; const ASortResultList: Boolean = True); overload;
    constructor Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    property SkippedFilesCount: Integer read FSkippedFilesCount write FSkippedFilesCount;
    property SortResultList: Boolean read FSortResultList write FSortResultList;
  end;

  TParallelFileScanner = class(TParallelFileScannerCustom)
  strict private
  public
    function GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes; var AFileNamesList: TStringList): Boolean; overload; override;
    function GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes; var AFileNamesList: TStringList): Boolean; overload; override;
  end;

implementation

uses
  System.Diagnostics
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, OtlTaskControl
{$ELSE}
  , System.Threading
{$ENDIF};

{ TFileScanExcludes }

procedure TFileScanExcludes.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFileScanExcludes.CacheUpperPrefixes;
begin
  SetLength(FUpperPathPrefixes, Length(FPathPrefixes));

  for var LIndex := Low(FPathPrefixes) to High(FPathPrefixes) do
    FUpperPathPrefixes[LIndex] := ExcludeTrailingPathDelimiter(FPathPrefixes[LIndex].ToUpper);

  SetLength(FUpperPathSuffixes, Length(FPathSuffixes));

  for var LIndex := Low(FPathSuffixes) to High(FPathSuffixes) do
    FUpperPathSuffixes[LIndex] := ExcludeTrailingPathDelimiter(FPathSuffixes[LIndex].ToUpper);
end;

procedure TFileScanExcludes.EndUpdate;
begin
  FUpdateCount := Max(0, FUpdateCount - 1);

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

function TFileScanExcludes.GetPathPrefixesString: string;
begin
  Result := Result.Join(';', FPathPrefixes);
end;

function TFileScanExcludes.GetPathSuffixesString: string;
begin
  Result := Result.Join(';', FPathSuffixes);
end;

function TFileScanExcludes.GetUpperPathPrefixes: TArray<string>;
begin
  Result := FUpperPathPrefixes;
end;

function TFileScanExcludes.GetUpperPathSuffixes: TArray<string>;
begin
  Result := FUpperPathSuffixes;
end;

procedure TFileScanExcludes.SetPathPrefixes(const Value: TArray<string>);
begin
  FPathPrefixes := Value;

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

procedure TFileScanExcludes.SetPathSuffixes(const Value: TArray<string>);
begin
  FPathSuffixes := Value;

  if FUpdateCount = 0 then
    CacheUpperPrefixes;
end;

{ TParallelFileScannerCustom }

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

  FSortResultList := ASortResultList;
  FLock := TCriticalSection.Create;
  FExtensions := TStringList.Create;

  FExtensions.AddStrings(AExtensions);
end;

destructor TParallelFileScannerCustom.Destroy;
begin
  FExtensions.Free;
  FLock.Free;

  inherited Destroy;;
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

function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes;
  var AFileNamesList: TStringList): Boolean;
var
  LCurrentRootPath: string;
  LListsOfFilelists: TObjectList<TStringList>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;
  FExclusions := AExcludes;

  FSkippedFilesCount := 0;

  LListsOfFilelists := TObjectList<TStringList>.Create(True);
  try
    for LCurrentRootPath in ADirectories do
    begin
      if not DirectoryExists(LCurrentRootPath) then
        Continue;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
      Parallel
        .&for(0, FExtensions.Count - 1)
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
          end;
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

function TParallelFileScannerCustom.GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes;
  var AFileNamesList: TStringList): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExcludes, AFileNamesList);
end;

procedure TParallelFileScannerCustom.GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption;
  const AFiles: TStringList);
var
  LPreCallback: TDirectoryWalkProc;
begin
  CheckGetFilesParameters(APath, ASearchPattern);

  AFiles.Capacity := 1024 * 16;

  LPreCallback :=
    function (const APath: string; const AFileInfo: TSearchRec): Boolean
    begin
      Result := True;

      if AFileInfo.Attr and System.SysUtils.faDirectory = 0 then
        AFiles.Add(TPath.Combine(APath, AFileInfo.Name, False));
    end;

  WalkThroughDirectory(APath, ASearchPattern, LPreCallback, ASearchOption = TSearchOption.soAllDirectories);
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
  LMatch: Boolean;
  LStop: Boolean;
begin
  if FindFirst(TPath.Combine(APath, '*', False), faAnyFile, LSearchRec) = 0 then
  try
    LStop := False;

    repeat
      if (LSearchRec.Name = CURRENT_DIR) or (LSearchRec.Name = PARENT_DIR) then
        Continue;

      LMatch := TPath.MatchesPattern(LSearchRec.Name, APattern, False);

      // call the pre-order callback method
      if LMatch and not ExcludedFilenameByuSuffix(TPath.Combine(APath, LSearchRec.Name, False)) then
      begin
        Inc(FSkippedFilesCount);

        LStop := not APreCallback(APath, LSearchRec);
      end;

      if not LStop then
      begin
        // go recursive in subdirectories
        if ARecursive and (LSearchRec.Attr and System.SysUtils.faDirectory <> 0) then
        begin
          var LNewPath := TPath.Combine(APath, LSearchRec.Name, False);

          if not ExcludedPathByPrefix(LNewPath) then
            WalkThroughDirectory(LNewPath, APattern, APreCallback, ARecursive)
          else
            Inc(FSkippedFilesCount); // TODO: These paths should be added to list, and count those files if SkippedFilesCount is queried
        end;
      end;
    until LStop or (FindNext(LSearchRec) <> 0);
  finally
    FindClose(LSearchRec);
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TStringList; const ASortResultList: Boolean = True);
begin
  Create(AExtensions.ToStringArray, ASortResultList);
end;

{ TParallelFileScanner }

function TParallelFileScanner.GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes;
  var AFileNamesList: TStringList): Boolean;
begin
  Result := inherited GetFileList(ADirectories, AExcludes, AFileNamesList);
end;

function TParallelFileScanner.GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes;
  var AFileNamesList: TStringList): Boolean;
begin
  Result := inherited GetFileList(ADirectories, AExcludes, AFileNamesList);
end;

end.
