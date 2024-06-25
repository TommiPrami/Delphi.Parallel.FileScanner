unit DPFSUnit.Parallel.FileScanner;

interface

{TODO: Could we move more code to the to the TParallelFileScannerCustom? Have to think about it.
       Still way too much duplicate code between "reguar" and spring version }

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.IOUtils, System.SyncObjs, System.SysUtils, System.Generics.Collections;

type
  TDirectoryWalkProc = reference to function (const APath: string; const AFileInfo: TSearchRec): Boolean;

  TFileScanExcludes = record
  strict private
    FPathPrefixes: TArray<string>;
    FPathSuffixes: TArray<string>;
    function GetPathPrefixesString: string;
    function GetPathSuffixesString: string;
    function GetUpperPathPrefixes: TArray<string>;
    function GetUpperPathSuffixes: TArray<string>;
  public
    property PathPrefixes: TArray<string> read FPathPrefixes write FPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write FPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
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
    FExtensions: TStringList;
    FLock: TCriticalSection;
    FSkippedFilesCount: Integer;
    procedure InternalCheckDirPathParam(const APath: string; const AExistsCheck: Boolean);
    procedure CheckGetFilesParameters(const APath: string; const ASearchPattern: string);
    procedure GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const AFiles: TStringList); virtual;
    procedure WalkThroughDirectory(const APath, APattern: string; const APreCallback: TDirectoryWalkProc; const ARecursive: Boolean);
    procedure FilterFileNames(const AFileNames: TStringList; const AExcludes: TFileScanExcludes; const AFilteredList: TObjectList<TStringList>); virtual;
    procedure MergeResultLists(const AResultList: TObjectList<TStringList>; const AResult: TStringList); virtual;
  public
    constructor Create(const AExtensions: TStringList); overload;
    constructor Create(const AExtensions: TArray<string>); overload;
    destructor Destroy; override;

    function GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes; var AFileNamesArray: TStringList): Boolean; overload; virtual;
    function GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes; var AFileNamesList: TStringList): Boolean; overload; virtual;

    property SkippedFilesCount: Integer read FSkippedFilesCount write FSkippedFilesCount;
    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
  end;

  TParallelFileScanner = class(TParallelFileScannerCustom)
  strict private
  public
  end;

implementation

uses
  System.Diagnostics
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, OtlTaskControl
{$ELSE}
  , System.Threading
{$ENDIF};

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

constructor TParallelFileScannerCustom.Create(const AExtensions: TArray<string>);
begin
  inherited Create;

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

procedure TParallelFileScannerCustom.FilterFileNames(const AFileNames: TStringList; const AExcludes: TFileScanExcludes;
  const AFilteredList: TObjectList<TStringList>);
var
  LIndex: Integer;
  LCurrentFilename: string;
  LCurrentExcludedDirectory: string;
  LCurrentExcludedFilename: string;
begin
  if (Length(AExcludes.PathPrefixes) > 0) or (Length(AExcludes.PathSuffixes) > 0) then
  begin
      for LIndex := AFileNames.Count - 1 downto 0 do
      begin
        LCurrentFilename := AFileNames[LIndex].ToUpper;

        var LExluded := False;

        for LCurrentExcludedDirectory in AExcludes.UpperPathPrefixes do
        begin
          if LCurrentExcludedDirectory = '' then
            Continue;

          if LCurrentFilename.StartsWith(LCurrentExcludedDirectory) then
          begin
            LExluded := True;
            Inc(FSkippedFilesCount);
            Break;
          end;
        end;

        if not LExluded then
        begin
          for LCurrentExcludedFilename in AExcludes.UpperPathSuffixes do
          begin
            if LCurrentExcludedFilename = '' then
              Continue;

            if LCurrentFilename.EndsWith(LCurrentExcludedFilename) then
            begin
              LExluded := True;
              Inc(FSkippedFilesCount);
              Break;
            end;
          end;
        end;

        if LExluded then
          AFileNames.Delete(LIndex);
      end;
  end;

  FLock.Acquire;
  try
    AFilteredList.Add(AFileNames);
  finally
    FLock.Release;
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

          FilterFileNames(LTempFileNames, AExcludes, LListsOfFilelists);
        end
      );
    end;

    FLock.Acquire;
    try
      MergeResultLists(LListsOfFilelists, AFileNamesList);
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
  var AFileNamesArray: TStringList): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExcludes, AFileNamesArray);
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
      LMatch := TPath.MatchesPattern(LSearchRec.Name, APattern, False);

      // call the pre-order callback method
      if LMatch and Assigned(APreCallback) then
        LStop := not APreCallback(APath, LSearchRec);

      if not LStop then
      begin
        // go recursive in subdirectories
        if ARecursive and (LSearchRec.Attr and System.SysUtils.faDirectory <> 0)
          and (LSearchRec.Name <> CURRENT_DIR) and (LSearchRec.Name <> PARENT_DIR) then
          WalkThroughDirectory(TPath.Combine(APath, LSearchRec.Name, False), APattern, APreCallback, ARecursive);
      end;
    until LStop or (FindNext(LSearchRec) <> 0);
  finally
    FindClose(LSearchRec);
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TStringList);
begin
  Create(AExtensions.ToStringArray);
end;

{ TFileScanExcludes }

function TFileScanExcludes.GetPathPrefixesString: string;
begin
  Result := Result.Join(';', FPathPrefixes);
end;

function TFileScanExcludes.GetPathSuffixesString: string;
begin
  Result := Result.Join(';', FPathSuffixes);
end;

function TFileScanExcludes.GetUpperPathPrefixes: TArray<string>;
var
  LLength: Integer;
  LIndex: Integer;
begin
  LLength := Length(FPathPrefixes);

  SetLength(Result, LLength);

  for LIndex := 0 to LLength - 1 do
    Result[LIndex] := FPathPrefixes[LIndex].ToUpper;
end;

function TFileScanExcludes.GetUpperPathSuffixes: TArray<string>;
var
  LLength: Integer;
  LIndex: Integer;
begin
  LLength := Length(FPathSuffixes);

  SetLength(Result, LLength);

  for LIndex := 0 to LLength - 1 do
    Result[LIndex] := FPathSuffixes[LIndex].ToUpper;
end;



end.
