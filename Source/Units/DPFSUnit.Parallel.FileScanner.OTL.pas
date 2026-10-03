unit DPFSUnit.Parallel.FileScanner.OTL;

// Windows only, like OmniThreadLibrary itself, so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
uses
  System.Classes, System.SysUtils, DPFSUnit.Parallel.FileScanner, OtlContainers, OtlTaskControl;

type
  // Scanner whose workers run on OmniThreadLibrary: pooled tasks that each scan owns and releases itself, in the
  // scanners' own pool, so scanning needs no message loop and back-to-back scans reuse the same threads. The pool
  // has as many threads as the largest scan so far has workers - one per core unless Workers says otherwise - and
  // never more than 56, so a scan never uses more than 56 workers, whatever Workers.Count says. Adds a GetFileList
  // that streams into an OTL value queue.
  TParallelFileScannerOTL = class(TParallelFileScannerCustom)
  strict protected
    function GetWorkerCount: Integer; override;
    procedure ExecuteWorkers(const AWorkerCount: Integer; const AWorker: TProc<Integer>;
      const APriority: TThreadPriority); override;
  public
    // TThreadPriority as OTL's TOTLThreadPriority. OTL has no time-critical priority: tpTimeCritical maps to
    // tpHighest.
    class function ToOTLThreadPriority(const APriority: TThreadPriority): TOTLThreadPriority; static;

    // Streams every matching file into AFileNamesOmniValueQueue as it is found (the queue is thread-safe);
    // AFileCount is the number of files queued. SortResultList does not apply here.
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesOmniValueQueue: TOmniQueue; var AFileCount: Integer): Boolean; overload;
  end;
{$IFEND}

implementation

{$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
uses
  Winapi.Windows, System.Diagnostics, System.Math, GpStuff, OtlCommon, OtlTask, OtlThreadPool;

const
  // Upper bound for workers - and so for the scanner pool's threads. The OTL pool's manager waits on one handle
  // per pool thread plus a few of its own, and must stay within WaitForMultipleObjects' 64 (see ScannerPool).
  MAX_OTL_WORKERS = 56;
  OTL_THREAD_PRIORITIES: array[TThreadPriority] of TOTLThreadPriority = (TOTLThreadPriority.tpIdle,
    TOTLThreadPriority.tpLowest, TOTLThreadPriority.tpBelowNormal, TOTLThreadPriority.tpNormal,
    TOTLThreadPriority.tpAboveNormal, TOTLThreadPriority.tpHighest, TOTLThreadPriority.tpHighest);

threadvar
  // Set while a thread runs a scanner worker: a scan started from there (a callback scanning too) runs inline.
  GInScannerWorker: Boolean;

var
  GScannerPool: IOmniThreadPool;
  GScannerPoolLock: TObject;

// The scanners' own OTL pool, with as many threads as the largest scan so far has workers (AWorkerCount, at most
// MAX_OTL_WORKERS): never more. Not GlobalParallelPool: that one has no thread limit, and when scans run back to back
// the next scan's tasks can arrive before the pool has put the previous scan's threads back on its idle list, so it
// starts new threads - past ~60 of them the pool's manager waits on more than 64 handles, and OTL's wait for more
// than 64 handles (TWaitFor in OtlSync.pas) can then crash the process. With MaxExecuting set, a task that finds no
// idle thread waits for one instead. Locked, as concurrent scans may each raise the limit.
function ScannerPool(const AWorkerCount: Integer): IOmniThreadPool;
begin
  TMonitor.Enter(GScannerPoolLock);
  try
    if not Assigned(GScannerPool) then
    begin
      GScannerPool := CreateThreadPool('Parallel.FileScanner pool');
      GScannerPool.MaxExecuting := AWorkerCount;
      GScannerPool.IdleWorkerThreadTimeout_sec := 60;
      GScannerPool.MaxQueuedTime_sec := 0;
    end
    else if GScannerPool.MaxExecuting < AWorkerCount then
      GScannerPool.MaxExecuting := AWorkerCount;

    Result := GScannerPool;
  finally
    TMonitor.Exit(GScannerPoolLock);
  end;
end;

// Wraps worker AWorkerIndex as an OTL task body. A function of its own so that every task captures its own index:
// closures/anonymous methods created in one loop all share - and so all see the last value of - the loop variable.
function CreateWorkerTaskDelegate(const AWorker: TProc<Integer>; const AWorkerIndex: Integer): TOmniTaskDelegate;
begin
  Result :=
    procedure(const ATask: IOmniTask)
    begin
      GInScannerWorker := True;
      try
        AWorker(AWorkerIndex);
      finally
        GInScannerWorker := False;
      end;
    end;
end;

{ TParallelFileScannerOTL }

class function TParallelFileScannerOTL.ToOTLThreadPriority(const APriority: TThreadPriority): TOTLThreadPriority;
begin
  Result := OTL_THREAD_PRIORITIES[APriority];
end;

function TParallelFileScannerOTL.GetWorkerCount: Integer;
begin
  Result := Min(inherited GetWorkerCount, MAX_OTL_WORKERS);
end;

procedure TParallelFileScannerOTL.ExecuteWorkers(const AWorkerCount: Integer; const AWorker: TProc<Integer>;
  const APriority: TThreadPriority);
var
  LPool: IOmniThreadPool;
  LTasks: TArray<IOmniTaskControl>;
begin
  // Started from a scanner worker (a callback scanning too): run the workers on this thread, one after another -
  // the first walks the whole tree, the rest find it done. Queuing them on the bounded pool could deadlock: the
  // outer walk's workers may hold every pool thread, waiting for this walk.
  if GInScannerWorker then
  begin
    for var LWorkerIndex := 0 to AWorkerCount - 1 do
      AWorker(LWorkerIndex);

    Exit;
  end;

  // One pooled task per worker, owned - and released - right here. Not Parallel.For: it makes its tasks
  // Unobserved, and OTL releases those only when the calling thread processes their termination messages, so
  // a caller that scans without pumping messages in between (a console app, a service, a worker thread, a GUI
  // loop of back-to-back scans) piled up ~0.6 MB and 7 handles per task - 28 tasks per scan - until it ran out
  // of memory.
  LPool := ScannerPool(AWorkerCount);
  SetLength(LTasks, AWorkerCount);
  try
    for var LWorkerIndex := 0 to AWorkerCount - 1 do
      LTasks[LWorkerIndex] := CreateTask(CreateWorkerTaskDelegate(AWorker, LWorkerIndex),
        'Parallel.FileScanner worker #' + LWorkerIndex.ToString)
        .SetPriority(ToOTLThreadPriority(APriority))
        .Schedule(LPool);
  finally
    // Every started worker must be done before the walk's state is freed - also when starting one of them failed.
    for var LTask in LTasks do
      if Assigned(LTask) then
        LTask.WaitFor(INFINITE);
  end;

  // As with TParallel.For, a worker's exception is raised in the caller.
  for var LTask in LTasks do
  begin
    var LException := LTask.DetachException;

    if Assigned(LException) then
      raise LException;
  end;
end;

function TParallelFileScannerOTL.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesOmniValueQueue: TOmniQueue; var AFileCount: Integer): Boolean;
var
  LFileScanStopWatch: TStopwatch;
  LFileCount: TGp4AlignedInt;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  AFileCount := 0;
  LFileCount.Value := 0;

  // Created inside the factory (not kept in a captured local) to avoid a self-referencing closure/anonymous method frame.
  RunParallelWalk(ADirectories, AExclusions, GetWorkerCount,
    function(AWorkerIndex: Integer): TDirectoryWalkProc
    begin
      Result :=
        procedure(const AFileName: string)
        var
          LOmniValue: TOmniValue;
        begin
          // IOmniValueQueue is thread-safe and LOmniValue is local to this callback,
          // so no external lock is needed around the enqueue.
          LOmniValue.AsString := AFileName;
          AFileNamesOmniValueQueue.Enqueue(LOmniValue);
          LFileCount.Increment;
        end;
    end,
    nil);

  AFileCount := LFileCount.Value;
  Result := AFileCount > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

initialization
  GScannerPoolLock := TObject.Create;

finalization
  GScannerPool := nil; // stops the pool's threads while OTL itself is still alive
  GScannerPoolLock.Free;
{$IFEND}

end.
