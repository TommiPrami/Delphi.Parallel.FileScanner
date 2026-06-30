unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, DPFSUnit.Parallel.FileScanner, Spring.Collections
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl
{$ENDIF};

type
  TParallelFileScannerSpring = class(TParallelFileScannerCustom)
  strict protected
    procedure MergeResultLists(const AResultList: IList<IList<string>>; const AResult: IList<string>); reintroduce;
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
  LListOfFileLists: IList<IList<string>>;
  LScanJobs: TArray<TScanJob>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  FExclusions := AExclusions;
  ResetCounters;
  CheckExtensions;

  LScanJobs := BuildScanJobs(ADirectories);

  LListOfFileLists := TCollections.CreateList<IList<string>>;

  if Length(LScanJobs) > 0 then
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  begin
    LTaskConfig := Parallel.TaskConfig;
    LTaskConfig.SetPriority(APriority);

    Parallel
      .&for(0, High(LScanJobs))
      .TaskConfig(LTaskConfig)
      .NumTasks(TThread.ProcessorCount)
      .Execute(
{$ELSE}
    TParallel
      .&for(0, High(LScanJobs),
{$ENDIF}
      procedure(AIndex: Integer)
      var
        LTempFileNames: IList<string>;
      begin
        LTempFileNames := TCollections.CreateList<string>;

        WalkThroughDirectory(LScanJobs[AIndex].Directory,
          procedure(const AFileName: string)
          begin
            LTempFileNames.Add(AFileName);
          end,
          LScanJobs[AIndex].Recursive);

        if LTempFileNames.Count > 0 then
        begin
          FLock.Acquire;
          try
            LListOfFileLists.Add(LTempFileNames);
          finally
            FLock.Release;
          end;
        end;
      end
    );
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  end;
{$ENDIF}

  // All worker tasks have finished here (Execute blocks), so no lock is needed.
  MergeResultLists(LListOfFileLists, AFileNamesList);

  if SortResultList then
    AFileNamesList.Sort;

  Result := AFileNamesList.Count > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
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

procedure TParallelFileScannerSpring.MergeResultLists(const AResultList: IList<IList<string>>;
  const AResult: IList<string>);
const
  MERGE_INITIAL_CAPACITY = 2000;
var
  LUniqueFiles: ISet<string>;
begin
  // Unordered set: Add already ignores duplicates, and the caller sorts the result
  // afterwards when SortResultList is set.
  LUniqueFiles := TCollections.CreateSet<string>;
  LUniqueFiles.Capacity := MERGE_INITIAL_CAPACITY;

  for var LList in AResultList do
    LUniqueFiles.AddRange(LList);

  AResult.AddRange(LUniqueFiles);
end;

{$ENDIF}

end.
