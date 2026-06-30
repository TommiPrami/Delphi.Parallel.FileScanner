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

unit Spring.HazardEra;

interface

// implementation based on the paper by Pedro Ramalhete and Andreia Correia
// https://github.com/pramalhe/ConcurrencyFreaks/blob/master/papers/hazarderas-2017.pdf

type
  GuardedPointer = record
  private
    ptr: Pointer;
    ThreadControlBlock: Pointer;
  public
    procedure Release;
    class operator Implicit(const value: GuardedPointer): Pointer; inline;
  end;

function AcquireGuard(var p; isFirstAttempt: Boolean = True): GuardedPointer;

procedure EraArraySetLength(var a; count: NativeInt; elemInfo: Pointer);
procedure EraArrayCopy(var target; source: Pointer);
procedure EraArrayClear(var a);
procedure EraArrayDelete(var target; index: NativeInt; count: NativeInt = 1);

implementation

uses
  Classes,
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  SyncObjs,
  Spring;

type
  TEra = Int64;

  PEraEntity = ^TEraEntity;
  TEraEntity = packed record
    newEra: TEra;
    delEra: TEra;
  end;

const
  Inactive = 0;
  Active = -1;
  None = 0;


{$REGION 'Utility routines'}

{$IFDEF MSWINDOWS}
function GetCurrentThreadID: TThreadID;
{$IFDEF ASSEMBLER}
asm
{$IFDEF CPUX86}
  mov eax,fs:[$24]
{$ELSE}
  mov eax,gs:[$48]
{$ENDIF}
end;
{$ELSE}
external 'kernel32.dll' name 'GetCurrentThreadId';
{$ENDIF}
{$ENDIF}

function GetMem_Aligned64(size: Integer): Pointer;
const
  Alignment = 64;
  AlignmentMask = Alignment - 1;
begin
  GetMem(Result, size + AlignmentMask);

  // memory allocated via this function for instances of THazardEraThreadControlBlock
  // must live until the end of the application because the hazard era mechanism
  // needs to be working until the end as well as there might be objects using it
  // outliving this unit - meaning the finalization of this unit might run earlier
  // than the deallocation of those objects
  RegisterExpectedMemoryLeak(Result);

  // align to 64 byte
  UIntPtr(Result) := (UIntPtr(Result) + AlignmentMask) and not AlignmentMask;
end;

{$IFDEF CPUX86}
function AtomicLoad(var source: Int64): Int64;
asm
  movq xmm0,[source]
  movd eax,xmm0
  psrldq xmm0,4
  movd edx,xmm0
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TThreadBlockList'}

const
  // size of used data in THazardEraThreadControlBlock
  DataSize =
    SizeOf(Pointer) +     // Next
    SizeOf(Pointer) +     // Prev
    SizeOf(NativeUInt) +  // ThreadId
    SizeOf(NativeInt) +   // Active
    SizeOf(TEra);         // Era
  CacheLineSize = 64;

type
  {$HINTS OFF}
  PHazardEraThreadControlBlock = ^THazardEraThreadControlBlock;
  THazardEraThreadControlBlock = record
  private
    Next, Prev: PHazardEraThreadControlBlock;
    ThreadId: NativeUInt;
    Active: NativeInt;
    Era: TEra;
  strict private
    // ensure that each block aligns to its own cache line
    Padding: array[1..CacheLineSize - DataSize] of Byte;
  end;

  TThreadBlockList = record
  private class var
    blocks: array[0..255] of PHazardEraThreadControlBlock;
    latest: PHazardEraThreadControlBlock;
  class threadvar
    activeBlock: PHazardEraThreadControlBlock;
  public
    class function Acquire(isFirstAttempt: Boolean): PHazardEraThreadControlBlock; static;
  end;

class function TThreadBlockList.Acquire(isFirstAttempt: Boolean): PHazardEraThreadControlBlock;

  function New(var era: PHazardEraThreadControlBlock; currentThreadId: TThreadID): PHazardEraThreadControlBlock;
  begin
    Result := GetMem_Aligned64(SizeOf(THazardEraThreadControlBlock));
    Result.ThreadId := currentThreadId;
    Result.Active := Active;
    Result.Era := Inactive;

    Result.Next := PHazardEraThreadControlBlock(AtomicExchange(Pointer(era), Pointer(Result)));
    Result.Prev := PHazardEraThreadControlBlock(AtomicExchange(Pointer(latest), Pointer(Result)));
    activeBlock := Result;
  end;

var
  currentThreadId: TThreadID;
  index: NativeUInt;
begin
  Result := activeBlock;
  if Assigned(Result) then
  begin
    if not isFirstAttempt then
      Exit;
    if AtomicExchange(Result.Active, Active) = Inactive then
      Exit;
  end;

  currentThreadId := GetCurrentThreadID;
  index := (currentThreadId xor (currentThreadId shr 8)) and High(blocks);

  Result := blocks[index];
  if Assigned(Result) then
  repeat
    if AtomicExchange(Result.Active, Active) = Inactive then
    begin
      Result.ThreadId := currentThreadId;
      activeBlock := Result;
      Exit;
    end;
    Result := Result.Next;
  until Result = nil;

  Result := New(blocks[index], currentThreadId);
end;

{$ENDREGION}


{$REGION 'THazardEra'}

var
  lock: TCriticalSection;
  eraClock: TEra;
  retiredList: TList;


function AcquireGuard(var p; isFirstAttempt: Boolean): GuardedPointer;
{$IFDEF CPUX86}
asm
  push ebx
  mov ebx, ecx
  mov eax, [eax]
  mov [ebx], eax
  mov eax, edx
  call TThreadBlockList.Acquire
  mov [ebx].GuardedPointer.ThreadControlBlock, eax
  movq xmm0, eraClock
  movq [eax].THazardEraThreadControlBlock.Era, xmm0
  pop ebx
end;
{$ELSE}
var
  current: PHazardEraThreadControlBlock;
begin
  Result.ptr := Pointer(p);
  current := TThreadBlockList.Acquire(isFirstAttempt);
  Result.ThreadControlBlock := current;
  current.Era := eraClock;
end;
{$ENDIF}

procedure GuardedPointer.Release;
{$IFDEF CPUX86}
asm
  mov eax, [eax].GuardedPointer.ThreadControlBlock
  pxor xmm0, xmm0
  movq [eax].THazardEraThreadControlBlock.Era, xmm0
  xor edx, edx
  mov [eax].THazardEraThreadControlBlock.Active, edx
end;
{$ELSE}
begin
  with PHazardEraThreadControlBlock(ThreadControlBlock)^ do
  begin
    Era := 0;
    Active := 0;
  end;
end;
{$ENDIF}

class operator GuardedPointer.Implicit(const value: GuardedPointer): Pointer;
begin
  Result := value.ptr;
end;

function Delete_Ptr(const obj: PEraEntity): TThreadId;
label
  Exit;
var
  info: PHazardEraThreadControlBlock;
  era: TEra;
begin
  info := TThreadBlockList.latest;
  while Assigned(info) do
  begin
    if info.Active <> Inactive then
    begin
      era := {$IFDEF CPUX86}AtomicLoad(info.Era){$ELSE}info.Era{$ENDIF};
      if (era >= obj.newEra) and (era <= obj.delEra) then
        goto Exit;
    end;
    info := info.Prev;
  end;
  FreeMem(obj);
  System.Exit(0);
Exit:
  Result := info.ThreadId;
end;

type
  TListHelper = class helper for TList
    procedure DeleteRange(Index, Count: NativeInt);
  end;

procedure TListHelper.DeleteRange(Index, Count: NativeInt);
var
  DeleteCount, TailCount: NativeInt;
begin
  DeleteCount := Count;
  with Self do
  begin
    Dec(FCount, DeleteCount);
    TailCount := FCount - Index;
    if TailCount > 0 then
      System.Move(FList[Index + DeleteCount], FList[Index], TailCount * SizeOf(Pointer));
  end;
end;

procedure Retire(p: PEraEntity);
var
  currEra: TEra;
  threadId, currentThreadId: TThreadID;
  i, index, count: Integer;
begin
  if Assigned(lock) then
  begin
    currEra := {$IFDEF CPUX86}AtomicLoad(eraClock){$ELSE}eraClock{$ENDIF};

   	lock.Acquire;
    try
      if Assigned(p) then
      begin
        p.delEra := currEra;
        retiredList.Add(p);
      end;

      if {$IFDEF CPUX86}AtomicLoad(eraClock){$ELSE}eraClock{$ENDIF} = currEra then
        AtomicIncrement(eraClock);

      currentThreadId := GetCurrentThreadID;
      index := 0;
      count := 0;
      for i := 1 to retiredList.Count do
      begin
        threadId := Delete_Ptr(retiredList.List[index + count]);
        if threadId <> 0 then
        begin
          if threadId = currentThreadId then Break;
          if count > 0 then
          begin
            retiredList.DeleteRange(index, count);
            count := 0;
          end;
          Inc(index);
        end
        else
          Inc(count);
      end;
      if count > 0 then
        retiredList.DeleteRange(index, count);
    finally
      lock.Release;
    end;
  end
  else
    FreeMem(p);
end;

{$ENDREGION}


{$REGION 'EraArray'}

type
  PEraArray = ^TEraArray;
  TEraArray = packed record
    newEra: TEra;
    delEra: TEra;
    elemInfo: PTypeInfo;
    Length: NativeInt;
  end;

procedure EraArraySetLength(var a; count: NativeInt; elemInfo: Pointer);
var
  p: PEraArray;
  oldSize, newSize: NativeInt;
begin
  if count = 0 then
  begin
    EraArrayClear(a);
    Exit;
  end;

  p := Pointer(a);
  oldSize := 0;
  if Assigned(p) then
  begin
    Dec(UIntPtr(p), SizeOf(TEraArray));
    oldSize := SizeOf(TEraArray) + p.Length * p.elemInfo.TypeSize;
  end;

  newSize := SizeOf(TEraArray) + count * PTypeInfo(elemInfo).TypeSize;
  ReallocMem(p, newSize);
  if newSize > oldSize then
    FillChar((PByte(p) + oldSize)^, newSize - oldSize, 0);
  p.newEra := {$IFDEF CPUX86}AtomicLoad(eraClock){$ELSE}eraClock{$ENDIF};
  p.elemInfo := elemInfo;
  p.Length := count;
  Inc(UIntPtr(p), SizeOf(TEraArray));
  Pointer(a) := p;
end;

procedure EraArrayCopy(var target; source: Pointer);
var
  p: PEraArray;
begin
  if source = nil then Exit;
  p := Pointer(UIntPtr(source) - SizeOf(TEraArray));
  Move(source^, Pointer(target)^, p.Length * p.elemInfo.TypeSize);
end;

procedure EraArrayClear(var a);
var
  p: PEraArray;
begin
  if Pointer(a) = nil then Exit;
  p := Pointer(UIntPtr(a) - SizeOf(TEraArray));
  Retire(PEraEntity(p));
  Pointer(a) := nil;
end;

procedure EraArrayDelete(var target; index, count: NativeInt);
var
  p: PEraArray;
  dest: Pointer;
  elemSize: Integer;
  tailCount: NativeInt;
begin
  if Pointer(target) = nil then Exit;
  p := Pointer(UIntPtr(target) - SizeOf(TEraArray));
  tailCount := p.Length - index - count;
  if tailCount > 0 then
  begin
    elemSize := p.elemInfo.TypeSize;
    dest := Pointer(PByte(target) + index * elemSize);
    Move(Pointer(PByte(target) + (index + count) * elemSize)^, Pointer(dest)^, tailCount * elemSize);
  end;
  EraArraySetLength(target, p.Length - count, p.elemInfo);
end;

{$ENDREGION}


initialization
  lock := TCriticalSection.Create;
  eraClock := 1;
  retiredList := TList.Create;

finalization
  Retire(nil);
  retiredList.Free;
  lock.Free;
  lock := nil;

end.
