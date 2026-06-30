{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2026 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Threading;

interface

uses
  Classes,
  Generics.Collections,
  SyncObjs,
  SysUtils,
  Spring;

type
  TRunTask = function(const proc: TProc): IInterface;
  TWaitForTask = procedure(const task: IInterface);

  ITask = interface;

  /// <summary>
  ///   Extension point for dealing with parallel computing inside the library.
  ///   This can be used to use other solutions to deal with parallel task
  ///   execution such as the PPL which is not available in all Delphi versions
  ///   supported by Spring4D. Provides a simple threadpool based default
  ///   implementation.
  /// </summary>
  /// <remarks>
  ///   Currently only used for TArray.Sort_Parallel, might be extended and used
  ///   in other places in the future.
  /// </remarks>
  ParallelAPI = record
  private
    // Schedules a task in the global threadpool
    class function InternalRun(const taskProc: TProc): ITask; static;

    // Waits for a task running in the global threadpool
    class procedure InternalWaitFor(const task: ITask); static;
  public class var
    RunTask: TRunTask;
    WaitFor: TWaitForTask;
  end;

// Following code was designed and contributed by Primoz Gabrijelcic

  /// <summary>
  ///   Raised by <see cref="TThreadPool.WaitFor" /> when more than one task
  ///   that was dequeued and executed while waiting raised an exception. The
  ///   original exception objects are owned by this instance (and freed with
  ///   it) and are available through the <see cref="InnerExceptions" />
  ///   property.
  /// </summary>
  EAggregateException = class(Exception)
  strict private
    fInnerExceptions: TArray<Exception>;
    function GetCount: Integer;
    function GetInnerException(index: Integer): Exception;
  public
    constructor Create(const innerExceptions: array of Exception);
    destructor Destroy; override;

    // Number of collected inner exceptions.
    property Count: Integer read GetCount;

    // The collected inner exceptions, owned by this instance.
    property InnerExceptions[index: Integer]: Exception read GetInnerException; default;
  end;

  ITask = interface ['{CA107748-CA21-4570-9615-7CFBB837FCDA}']
    function GetExceptObj: TObject;
    function GetIsCompleted: Boolean;

    // Executes the task. Internal use only.
    procedure Execute;

    // Stores exception raised in the task (if any).
    property ExceptObj: TObject read GetExceptObj;

    // Returns if the task is done.
    property IsCompleted: Boolean read GetIsCompleted;
  end;

  TThreadPool = class sealed
  strict private type
    TWorkerThread = class sealed(TThread)
    strict private
      fOwner: TThreadPool;
      fSignal: TEvent;
      fIsActive: Boolean;
    protected
      procedure TerminatedSet; {$IFNDEF DELPHIXE}override;{$ENDIF}
    public
      constructor Create(const owner: TThreadPool); overload;
      destructor Destroy; override;
      {$IFDEF DELPHIXE}
      procedure Terminate;
      {$ENDIF}
      procedure Execute; override;
      property IsActive: Boolean read fIsActive write fIsActive;
      property Signal: TEvent read fSignal;
    end;

    TTaskQueue = class(TQueue<ITask>)
      function TryDequeue(var item: ITask): Boolean;
    end;

    TTask = class(TRefCountedObject, ITask)
    strict private
      fExceptObj: TObject;
      fTaskProc: TProc;
      fWorkDone: TEvent;
      function GetExceptObj: TObject;
      function GetIsCompleted: Boolean;
    public
      constructor Create(const taskProc: TProc);
      destructor Destroy; override;
      procedure Execute;
      property ExceptObj: TObject read GetExceptObj;
    end;
  strict private
    fTasks: TTaskQueue;
    fThreads: TArray<TWorkerThread>;
    fInactiveThreads: TArray<TWorkerThread>;
    fInactiveTop: NativeInt;
    procedure EnterLock; inline;
    procedure LeaveLock; inline;
  strict protected //thread-unsafe functions, must be called only when internal data is acquired

    // Marks a thread active/inactive
    procedure MarkThreadActive(thread: TWorkerThread; isActive: Boolean);

  protected //thread-safe functions

    // Atomically gets next task and sets the thread 'active' state
    // (to True if the task was available, False if not).
    // Returns True if the task was available.
    // If the function returns False, 'task' is guaranteed to be 'nil'.
    function TryAllocateTask(thread: TWorkerThread; var task: ITask): Boolean;

    // Atomically retrieves one inactive thread and marks it active.
    // Returns False and 'nil' in the 'thread' parameter if there are no
    // inactive threads.
    function ActivateInactiveThread(var thread: TWorkerThread): Boolean;

    // Puts a task into the execution queue.
    procedure QueueTask(const task: ITask);
  public
    constructor Create(numThreads: Integer);
    destructor Destroy; override;

    function Run(const taskProc: TProc): ITask; overload;
    procedure WaitFor(const task: ITask);
  end;

implementation


{$REGION 'EAggregateException'}

constructor EAggregateException.Create(const innerExceptions: array of Exception);
var
  i: Integer;
  msg: string;
begin
  SetLength(fInnerExceptions, Length(innerExceptions));
  msg := Format('%d task(s) raised an exception while waiting',
    [Length(innerExceptions)]);
  for i := 0 to High(innerExceptions) do
  begin
    fInnerExceptions[i] := innerExceptions[i];
    msg := msg + sLineBreak + '  - ' + innerExceptions[i].ClassName + ': ' +
      innerExceptions[i].Message;
  end;
  inherited Create(msg);
end;

destructor EAggregateException.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fInnerExceptions) do
    fInnerExceptions[i].Free;
  inherited;
end;

function EAggregateException.GetCount: Integer;
begin
  Result := Length(fInnerExceptions);
end;

function EAggregateException.GetInnerException(index: Integer): Exception;
begin
  Result := fInnerExceptions[index];
end;

{$ENDREGION}


{$REGION 'TThreadPool'}

constructor TThreadPool.Create(numThreads: Integer);
var
  i: NativeInt;
  thread: TWorkerThread;
begin
  Assert(numThreads > 0);
  fTasks := TTaskQueue.Create;
  SetLength(fInactiveThreads, numThreads);
  SetLength(fThreads, numThreads);

  for i := 1 to numThreads do
  begin
    thread := TWorkerThread.Create(Self);
    thread.IsActive := False;
    fThreads[i - 1] := thread;
    fInactiveThreads[i - 1] := thread;
  end;
  fInactiveTop := High(fInactiveThreads);
end;

destructor TThreadPool.Destroy;
var
  i: NativeInt;
begin
  for i := 0 to DynArrayHigh(fThreads) do
    fThreads[i].Terminate;
  for i := 0 to DynArrayHigh(fThreads) do
    fThreads[i].Free;
  fTasks.Free;
end;

procedure TThreadPool.EnterLock;
begin
  MonitorEnter(Self);
end;

procedure TThreadPool.LeaveLock;
begin
  MonitorExit(Self);
end;

function TThreadPool.ActivateInactiveThread(var thread: TWorkerThread): Boolean;
begin
  EnterLock;
  Result := fInactiveTop >= 0;
  if Result then
  begin
    thread := fInactiveThreads[fInactiveTop];
    Dec(fInactiveTop);
    MarkThreadActive(thread, True);
  end
  else
    thread := nil;
  LeaveLock;
end;

procedure TThreadPool.MarkThreadActive(thread: TWorkerThread; isActive: Boolean);
var
  i: NativeInt;
begin
  if isActive <> thread.IsActive then
  begin
    thread.IsActive := isActive;
    if not isActive then
    begin
      Inc(fInactiveTop);
      fInactiveThreads[fInactiveTop] := thread;
    end
    else
      for i := 0 to fInactiveTop do
        if fInactiveThreads[i] = thread then
        begin
          if i <> fInactiveTop then
            fInactiveThreads[i] := fInactiveThreads[fInactiveTop];
          Dec(fInactiveTop);
          Break;
        end;
  end;
end;

procedure TThreadPool.QueueTask(const task: ITask);
begin
  EnterLock;
  fTasks.Enqueue(task);
  LeaveLock;
end;

function TThreadPool.Run(const taskProc: TProc): ITask;
var
  thread: TWorkerThread;
begin
  Result := TTask.Create(taskProc);

  QueueTask(Result);

  if ActivateInactiveThread(thread) then
    thread.Signal.SetEvent;
end;

function TThreadPool.TryAllocateTask(thread: TWorkerThread; var task: ITask): Boolean;
begin
  EnterLock;
  MarkThreadActive(thread, fTasks.TryDequeue(task));
  LeaveLock;
  Result := Assigned(task);
end;

procedure TThreadPool.WaitFor(const task: ITask);
var
  newTask: ITask;
  exceptions: TList<Exception>;
begin
  // While waiting we dequeue and run queued tasks. If a dequeued task raises,
  // we must NOT surface that exception until the task we are waiting for has
  // completed - otherwise the in-flight waited task is abandoned while the
  // caller unwinds. Collect every dequeued exception, finish the wait, then
  // report all that occurred so that none are lost: the waited task's own
  // exception keeps precedence (it is reported first), followed by the dequeued
  // ones. A lone exception is re-raised as is; several are surfaced together
  // inside an EAggregateException. The exception objects are owned via
  // AcquireExceptionObject and are not freed by TTask.Destroy, so each one we
  // keep is either re-raised or handed off to EAggregateException, which owns
  // and later frees them.
  exceptions := nil;
  try
    while not task.IsCompleted do
    begin
      EnterLock;
      fTasks.TryDequeue(newTask);
      LeaveLock;
      if not Assigned(newTask) then
        TThread.Yield
      else
      begin
        newTask.Execute;
        if Assigned(newTask.ExceptObj) then
        begin
          if not Assigned(exceptions) then
            exceptions := TList<Exception>.Create;
          exceptions.Add(Exception(newTask.ExceptObj));
        end;
      end;
    end;

    // The waited task is fully done. Report its own exception (if any) first -
    // it keeps precedence - together with every dequeued one. None may be lost.
    if Assigned(task.ExceptObj) then
    begin
      if not Assigned(exceptions) then
        exceptions := TList<Exception>.Create;
      exceptions.Insert(0, Exception(task.ExceptObj));
    end;

    if Assigned(exceptions) then
      if exceptions.Count = 1 then
        raise exceptions[0]
      else
        raise EAggregateException.Create(exceptions.ToArray);
  finally
    exceptions.Free;
  end;
end;

{$ENDREGION}


{$REGION 'TThreadPool.TWorkerThread'}

constructor TThreadPool.TWorkerThread.Create(const owner: TThreadPool);
begin
  inherited Create(False);
  fOwner := owner;
  fSignal := TEvent.Create(nil, False, False, '');
end;

destructor TThreadPool.TWorkerThread.Destroy;
begin
  inherited;
  fSignal.Free;
end;

procedure TThreadPool.TWorkerThread.Execute;
var
  task: ITask;
begin
  NameThreadForDebugging('ComputeCore worker');
  while not Terminated and (fSignal.WaitFor <> wrTimeout) do
    while not Terminated and fOwner.TryAllocateTask(Self, task) do
      task.Execute;
end;

{$IFDEF DELPHIXE}
procedure TThreadPool.TWorkerThread.Terminate;
begin
  inherited Terminate;
  TerminatedSet;
end;
{$ENDIF}

procedure TThreadPool.TWorkerThread.TerminatedSet;
begin
  fSignal.SetEvent;
end;

{$ENDREGION}


{$REGION 'TThreadPool.TTaskQueue'}

function TThreadPool.TTaskQueue.TryDequeue(var item: ITask): Boolean;
begin
  if Count > 0 then
  begin
    item := Dequeue;
    Exit(True);
  end;
  item := nil;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TThreadPool.TTask'}

constructor TThreadPool.TTask.Create(const taskProc: TProc);
begin
  fTaskProc := taskProc;
  fWorkDone := TEvent.Create;
end;

destructor TThreadPool.TTask.Destroy;
begin
  fWorkDone.WaitFor;
  fWorkDone.Free;
end;

procedure TThreadPool.TTask.Execute;
begin
  try
    try
      fTaskProc();
    except
      on E: Exception do
        FExceptObj := AcquireExceptionObject;
    end;
  finally
    fWorkDone.SetEvent;
  end;
end;

function TThreadPool.TTask.GetExceptObj: TObject;
begin
  Result := FExceptObj;
end;

function TThreadPool.TTask.GetIsCompleted: Boolean;
begin
  Result := fWorkDone.WaitFor(0) = wrSignaled;
end;

{$ENDREGION}


{$REGION 'TTask'}

var
  GlobalThreadPool: TThreadPool;

class function ParallelAPI.InternalRun(const taskProc: TProc): ITask;
var
  newThreadPool: TThreadPool;
begin
  if not Assigned(GlobalThreadPool) then
  begin
    Assert(NativeUInt(@GlobalThreadPool) mod SizeOf(pointer) = 0, 'TTask.Run: storage is not properly aligned!');
    Assert(NativeUInt(@newThreadPool) mod SizeOf(pointer) = 0, 'TTask.Run: temp is not properly aligned!');
    newThreadPool := TThreadPool.Create(CPUCount);
    if AtomicCmpExchange(Pointer(GlobalThreadPool), Pointer(newThreadPool), nil) <> nil then
      newThreadPool.Free;
  end;

  Result := GlobalThreadPool.Run(taskProc);
end;

class procedure ParallelAPI.InternalWaitFor(const task: ITask);
begin
  GlobalThreadPool.WaitFor(task);
end;

{$ENDREGION}


procedure InitializeParallelAPI;
begin
  ParallelAPI.RunTask := @ParallelAPI.InternalRun;
  ParallelAPI.WaitFor := @ParallelAPI.InternalWaitFor;
end;

initialization
  InitializeParallelAPI;

finalization
  GlobalThreadPool.Free;

end.
