unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, System.IOUtils, DPFSUnit.Parallel.FileScanner, Spring.Collections
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl
{$ENDIF};
type

  TParallelFileScannerSpring = class(TParallelFileScannerCustom)
  strict protected
    procedure GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const AFiles: IList<string>); reintroduce;
    procedure MergeResultLists(const AResultList: IList<IList<string>>; const AResult: IList<string>); reintroduce;
    procedure DoConvertRelativePathsToAbsolute(const AFileNames: IList<string>); reintroduce;
  public
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const AFileNamesList: IList<string>
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
        ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; reintroduce; overload;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions; const AFileNamesList: IList<string>
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
        ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; reintroduce; overload;
  end;
{$ENDIF}

implementation

{$IFDEF USE_SPRING4D}
uses
  System.Diagnostics, System.SysUtils
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask
{$ELSE}
  , System.Threading
{$ENDIF};

{ TParallelFileScannerSpring }

function TParallelFileScannerSpring.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IList<string>
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
var
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  LTaskConfig: IOmniTaskConfig;
{$ENDIF}
  LCurrentRootPath: string;
  LListOfFileLists: IList<IList<string>>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  FExclusions := AExclusions;
  ResetCounters;

  LListOfFileLists := TCollections.CreateList<IList<string>>;
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
          LTempFileNames: IList<string>;
        begin
          LExtension := FExtensions[AIndex];

          LTempFileNames := TCollections.CreateList<string>;

          // This GetFiles was ripped from the RTL and ported to use a Spring container. MAYBE it is not faster after all.
          // Should make two versions, one using the standard RTL and one using this, then time
          // them to see which is faster. This is now about 450ms so maybe no use to optimize much.
          GetFiles(LCurrentRootPath, LExtension, TSearchOption.soAllDirectories, LTempFileNames);

          if ConvertRelativePathsToAbsolute then
            DoConvertRelativePathsToAbsolute(LTempFileNames);

          if LTempFileNames.Count > 0 then
          begin
            FLock.Acquire;
            try
              LListOfFileLists.Add(LTempFileNames);
            finally
              FLock.Release;
            end;
          end;

          LTempFileNames := nil;
        end
      );
    end;

    FLock.Acquire;
    try
      MergeResultLists(LListOfFileLists, AFileNamesList);

      if SortResultList then
        AFileNamesList.Sort;
    finally
      FLock.Release;
    end;
  finally
    LListOfFileLists := nil;
  end;

  Result := AFileNamesList.Count > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

procedure TParallelFileScannerSpring.DoConvertRelativePathsToAbsolute(const AFileNames: IList<string>);
begin
  for var LIndex := 0 to AFileNames.Count - 1 do
  begin
    var LCurrentFileName := AFileNames[LIndex];

    if TPath.IsRelativePath(LCurrentFileName) then
      AFileNames[LIndex] := TPath.GetFullPath(LCurrentFileName);
  end;
end;

function TParallelFileScannerSpring.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IList<string>
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , APriority
{$ENDIF});
end;

procedure TParallelFileScannerSpring.GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption;
  const AFiles: IList<string>);
var
  LPreCallback: TDirectoryWalkProc;
begin
  CheckGetFilesParameters(APath, ASearchPattern);

  AFiles.Capacity := 1024 * 16;

  LPreCallback :=
    procedure (const AFileName: string)
    begin
      AFiles.Add(AFileName);
    end;

  WalkThroughDirectory(APath, ASearchPattern, LPreCallback, ASearchOption = TSearchOption.soAllDirectories);
end;

procedure TParallelFileScannerSpring.MergeResultLists(const AResultList: IList<IList<string>>;
  const AResult: IList<string>);
var
  I: Integer;
  J: Integer;
  LUniqueFiles: ISet<string>;
begin
  // Unordered set: Add already ignores duplicates, and the caller sorts the result
  // afterwards when SortResultList is set.
  LUniqueFiles := TCollections.CreateSet<string>;
  LUniqueFiles.Capacity := 2000;

  for I := 0 to AResultList.Count - 1 do
    for J := 0 to AResultList[I].Count - 1 do
      LUniqueFiles.Add(AResultList[I][J]);

  AResult.AddRange(LUniqueFiles);
end;

{$ENDIF}

end.
