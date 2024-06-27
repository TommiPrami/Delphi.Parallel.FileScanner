unit DPFSUnit.Parallel.FileScanner;

interface

{TODO: Could we move more code to the to the TParallelFileScannerCustom? Have to think about it.
       Still way too much duplicate code between "reguar" and Spring4D version }

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.IOUtils, System.Math, System.SyncObjs, System.SysUtils, System.Generics.Collections;

type
  TDirectoryWalkProc = reference to procedure(const AFileNamePath: string);

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
    class operator Initialize(out Dest: TFileScanExcludes);
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
    constructor Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True); overload;
    constructor Create(const AExtensions: TStringList; const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    property SkippedFilesCount: Integer read GetSkippedFilesCount write FSkippedFilesCount;
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

class operator TFileScanExcludes.Initialize(out Dest: TFileScanExcludes);
begin
  Dest.FUpdateCount := 0;
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
