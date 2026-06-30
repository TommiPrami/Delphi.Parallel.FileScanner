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

unit Spring.Container.CreationContext;

interface

uses
  Rtti,
{$IFDEF POSIX}
  Posix.Errno,
  Posix.SysTypes,
{$ENDIF}
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

{$IFDEF MSWINDOWS}
type
  SRWLOCK = record
    Ptr: Pointer;
  end;

procedure AcquireSRWLockShared(var Lock: SRWLock); stdcall external 'Kernel32.dll';
procedure AcquireSRWLockExclusive(var Lock: SRWLock); stdcall external 'Kernel32.dll';
function TryAcquireSRWLockShared(var Lock: SRWLock): Boolean; stdcall external 'Kernel32.dll';
procedure ReleaseSRWLockShared(var Lock: SRWLock); stdcall external 'Kernel32.dll';
procedure ReleaseSRWLockExclusive(var Lock: SRWLock); stdcall external 'Kernel32.dll';
{$ENDIF}

type
  TSlimRWLock = packed record
  private
    {$IFDEF MSWINDOWS}
    fLock: SRWLock;
    {$ENDIF MSWINDOWS}
    {$IFDEF POSIX}
    fLock: pthread_rwlock_t;
    {$ENDIF POSIX}
    fWriteThread: TThreadID;
    fWriteLocks: Integer;
  public
    procedure Initialize; inline;
    procedure EnterRead;
    procedure EnterWrite;
    procedure LeaveRead;
    procedure LeaveWrite;
    function TryBeginRead: Boolean;
  end;

type
  TCreationContext = class(TRefCountedObject, IInterface, ICreationContext)
  private
    fResolutionStack: IStack<TComponentModel>;
    fModel: TComponentModel;
    fArguments: IList<TValue>;
    fPerResolveInstances: IDictionary<TComponentModel, TValue>;
    fNamedArguments: IList<TNamedValue>;
    fTypedArguments: IList<TTypedValue>;
    fLock: TSlimRWLock;
  public
    constructor Create(const model: TComponentModel;
      const arguments: array of TValue);
    destructor Destroy; override;

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue;

    function EnterResolution(const model: TComponentModel;
      var instance: TValue): Boolean;
    procedure LeaveResolution(const model: TComponentModel);

    function AddArgument(const argument: TValue): Integer;
    procedure RemoveTypedArgument(index: Integer);
    procedure AddPerResolve(const model: TComponentModel; const instance: TValue);
    function TryHandle(const injection: IInjection;
      var handled: IInjection): Boolean;
  end;

implementation

uses
  TypInfo,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Container.Common,
  Spring.Container.Injection,
  Spring.Container.ResourceStrings,
  Spring.Reflection;

procedure TSlimRWLock.Initialize;
begin
  {$IFDEF POSIX}
  fLock := PTHREAD_RWLOCK_INITIALIZER;
  {$ENDIF}
end;

procedure TSlimRWLock.EnterRead;
begin
  if fWriteThread <> GetCurrentThreadId then
    {$IFDEF MSWINDOWS}
    AcquireSRWLockShared(fLock);
    {$ENDIF}
    {$IFDEF POSIX}
    CheckOSError(pthread_rwlock_rdlock(fLock));
    {$ENDIF}
end;

procedure TSlimRWLock.EnterWrite;
var
  currentThreadId: TThreadID;
begin
  currentThreadId := GetCurrentThreadId;
  if fWriteThread <> currentThreadId then
  begin
    {$IFDEF MSWINDOWS}
    AcquireSRWLockExclusive(fLock);
    {$ENDIF}
    {$IFDEF POSIX}
    CheckOSError(pthread_rwlock_wrlock(fLock));
    {$ENDIF}
    fWriteThread := currentThreadId;
  end;
  Inc(fWriteLocks);
end;

procedure TSlimRWLock.LeaveRead;
begin
  if fWriteThread <> GetCurrentThreadId then
    {$IFDEF MSWINDOWS}
    ReleaseSRWLockShared(fLock);
    {$ENDIF}
    {$IFDEF POSIX}
    CheckOSError(pthread_rwlock_unlock(fLock));
    {$ENDIF}
end;

procedure TSlimRWLock.LeaveWrite;

  procedure RaiseRemovedUnownedWriteLock;
  begin
    raise Exception.Create('Tried to remove a writelock which was not owned by the current thread');
  end;

var
  writeLocks: Integer;
begin
  if fWriteThread <> GetCurrentThreadId then
    RaiseRemovedUnownedWriteLock;

  writeLocks := fWriteLocks - 1;
  fWriteLocks := writeLocks;
  if writeLocks = 0 then
  begin
    fWriteThread := 0;
    {$IFDEF MSWINDOWS}
    ReleaseSRWLockExclusive(fLock);
    {$ENDIF}
    {$IFDEF POSIX}
    CheckOSError(pthread_rwlock_unlock(fLock));
    {$ENDIF}
  end;
end;

function TSlimRWLock.TryBeginRead: Boolean;
{$IFDEF POSIX}
var
  res: Integer;
{$ENDIF}
begin
  if fWriteThread = GetCurrentThreadId then
    Result := True
  else
  begin
    {$IFDEF MSWINDOWS}
    Result := TryAcquireSRWLockShared(fLock);
    {$ENDIF}
    {$IFDEF POSIX}
    res := pthread_rwlock_tryrdlock(fLock);
    Result := res = 0;
    if (not Result) and (res <> EBUSY) then
      CheckOSError(res);
    {$ENDIF}
  end;
end;

type
  TInterfacedObjectAccess = class(TInterfacedObject);


{$REGION 'TCreationContext'}

constructor TCreationContext.Create(const model: TComponentModel;
  const arguments: array of TValue);
var
  i: Integer;
begin
  fLock.Initialize;
  fModel := model;
  for i := Low(arguments) to High(arguments) do
    AddArgument(arguments[i]);
end;

destructor TCreationContext.Destroy;
var
  instance: TPair<TComponentModel, TValue>;
begin
  if Assigned(fPerResolveInstances) then
    for instance in fPerResolveInstances do
      if (instance.Key.LifetimeType = TLifetimeType.PerResolve)
        and (instance.Value.Kind = tkClass)
        and (TObject(TValueData(instance.Value).FAsObject) is TInterfacedObject) then
        TInterfacedObjectAccess(TValueData(instance.Value).FAsObject)._Release;
end;

function TCreationContext.AddArgument(const argument: TValue): Integer;
begin
  fLock.EnterWrite;
  try
    if argument.IsType(TypeInfo(TTypedValue)) then
    begin
      if not Assigned(fTypedArguments) then
        fTypedArguments := TCollections.CreateList<TTypedValue>;
      Result := fTypedArguments.Add(argument);
    end
    else if argument.IsType(TypeInfo(TNamedValue)) then
    begin
      if not Assigned(fNamedArguments) then
        fNamedArguments := TCollections.CreateList<TNamedValue>;
      Result := fNamedArguments.Add(argument);
    end
    else
    begin
      if not Assigned(fArguments) then
        fArguments := TCollections.CreateList<TValue>;
      Result := fArguments.Add(argument);
    end;
  finally
    fLock.LeaveWrite;
  end;
end;

procedure TCreationContext.AddPerResolve(const model: TComponentModel;
  const instance: TValue);
begin
  fLock.EnterWrite;
  try
    if not Assigned(fPerResolveInstances) then
      fPerResolveInstances := TCollections.CreateDictionary<TComponentModel, TValue>;

    fPerResolveInstances.Add(model, instance);
    if (model.LifetimeType = TLifetimeType.PerResolve)
      and (instance.Kind = tkClass)
      and (TObject(TValueData(instance).FAsObject) is TInterfacedObject) then
      TInterfacedObjectAccess(TValueData(instance).FAsObject)._AddRef;
  finally
    fLock.LeaveWrite;
  end;
end;

function TCreationContext.CanResolve(const context: ICreationContext; //FI:O804
  const dependency: TDependencyModel;
  const argument: TValue): Boolean; //FI:O804
var
  i: Integer;
begin
  fLock.EnterRead;
  try
    if Assigned(fTypedArguments) then
      with fTypedArguments.AsSpan do
        for i := Length - 1 downto 0 do // check most recently added first
          if SameTypeInfo(Items[i].TypeInfo, dependency.TypeInfo) then
            Exit(True);
    Result := False;
  finally
    fLock.LeaveRead;
  end;
end;

function TCreationContext.TryHandle(const injection: IInjection;
  var handled: IInjection): Boolean;
var
  arguments: TArray<TValue>;
  i, n, argCount: Integer;
  parameters: TArray<TRttiParameter>;
  value: ^TNamedValue;
begin
  fLock.EnterRead;
  try
    arguments := injection.Arguments;
    DynArrayUnique(Pointer(arguments), TypeInfo(TArray<TValue>));
    if not fModel.ConstructorInjections.Contains(injection) then
    begin
      handled := injection;
      Exit(True);
    end;

    parameters := injection.Target.AsMethod.GetParameters;
    // RTTI cannot handle open array parameters
    for i := Low(parameters) to High(parameters) do
      if pfArray in parameters[i].Flags then
        Exit(False);

    argCount := 0;
    if Assigned(fArguments) then
      argCount := fArguments.Count;

    if argCount > 0 then
      if Length(parameters) = argCount then
      begin
        // arguments for ctor are provided and count is correct
        with fArguments.AsSpan do
          for i := Low(parameters) to High(parameters) do // check all parameters
            if Items[i].IsType(parameters[i].ParamType.Handle) then
              arguments[i] := Items[i]^
            else
              Exit(False); // argument and parameter types did not match
      end
      else
        Exit(False);

    if Assigned(fNamedArguments) then
      with fNamedArguments.AsSpan do // check all named arguments
        for n := 0 to Length - 1 do
        begin
          value := Pointer(Items[n]);
          Result := False;
          for i := Low(parameters) to High(parameters) do
          begin // look for parameter that matches the name and type
            if parameters[i].HasName(value.Name)
              and value.Value.IsType(parameters[i].ParamType.Handle) then
            begin
              arguments[i] := value.Value;
              Result := True;
              Break;
            end;
          end;
          if not Result then // named argument was not found
            Exit;
        end;
  finally
    fLock.LeaveRead;
  end;

  Result := True; // all parameters are handled - create new injection
  handled := TConstructorInjection.Create(injection, arguments);
end;

function TCreationContext.EnterResolution(const model: TComponentModel;
  var instance: TValue): Boolean;
begin
  Result := False;
  fLock.EnterWrite;
  try
    if not Assigned(fModel) then // set the model if we don't know it yet
      fModel := model;
    if Assigned(fPerResolveInstances) and fPerResolveInstances.TryGetValue(model, instance) then
      Exit;
    if Assigned(fResolutionStack) and fResolutionStack.Contains(model) then
      if model.LifetimeType in [TLifetimeType.Singleton,
        TLifetimeType.PerResolve, TLifetimeType.SingletonPerThread] then
        Exit
      else
        raise ECircularDependencyException.CreateResFmt(
          @SCircularDependencyDetected, [model.ComponentTypeName]);
    if not Assigned(fResolutionStack) then
      fResolutionStack := TCollections.CreateStack<TComponentModel>;
    fResolutionStack.Push(model);
    Result := True;
  finally
    if not Result then
      fLock.LeaveWrite;
  end;
end;

procedure TCreationContext.LeaveResolution(const model: TComponentModel);
begin
  try
    if Assigned(fResolutionStack) and (fResolutionStack.Pop <> model) then
      raise EResolveException.CreateRes(@SResolutionStackUnbalanced);
  finally
    fLock.LeaveWrite;
  end;
end;

procedure TCreationContext.RemoveTypedArgument(index: Integer);
begin
  fLock.EnterWrite;
  try
    fTypedArguments.Delete(index);
  finally
    fLock.LeaveWrite;
  end;
end;

function TCreationContext.Resolve(const context: ICreationContext; //FI:O804
  const dependency: TDependencyModel;
  const argument: TValue): TValue; //FI:O804
var
  i: Integer;
begin
  fLock.EnterRead;
  try
    if Assigned(fTypedArguments) then
      for i := fTypedArguments.Count - 1 downto 0 do
        if SameTypeInfo(fTypedArguments[i].TypeInfo, dependency.TypeInfo) then
        begin
          Result := fTypedArguments[i].Value;
          TValueData(Result).FTypeInfo := dependency.TypeInfo;
          Exit;
        end;
  finally
    fLock.LeaveRead;
  end;
  raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
end;

{$ENDREGION}


end.
