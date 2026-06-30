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

unit Spring.Patches.RSP9815;

interface

implementation

{$O+,W-R-,T-,X+,H+,B-}

{$IFNDEF DELPHIX_RIO_UP}{$IFDEF MSWINDOWS}
uses
  Generics.Collections,
  Rtti,
  SyncObjs,
  SysUtils,
  TypInfo,
  Windows;

type
  TRttiPackagePatch = class(TRttiNamedObject)
  protected
    FLock: {$IFDEF DELPHIXE7_UP}TObject{$ELSE}TCriticalSection{$ENDIF};
    FHandleToObject: TDictionary<Pointer,TRttiObject>;
    {$IFDEF DELPHIXE2_UP}
    FBaseAddress: Pointer;
    {$ENDIF}
  end;

  TRealPackagePatch = class(TRttiPackagePatch)
  private
    FTypeInfo: PPackageTypeInfo;
    FTypeToName: TDictionary<PTypeInfo,string>;
    FNameToType: TDictionary<string,PTypeInfo>;

    procedure MakeTypeLookupTable;
  end;

  {$IF not declared(IntPtr)}
  IntPtr = NativeInt;
  {$IFEND}

  {$IF not declared(SIZE_T)}
  SIZE_T = DWORD;
  {$IFEND}

procedure PeekData(var P: PByte; var Data; Len: Integer);
begin
  Move(P^, Data, Len);
end;

procedure ReadData(var P: PByte; var Data; Len: Integer);
begin
  PeekData(P, Data, Len);
  Inc(P, Len);
end;

function ReadU8(var P: PByte): Byte;
begin
  ReadData(P, Result, SizeOf(Result));
end;

{$IFDEF DELPHIXE3_UP}
function _UTF8ToString(P: pointer): string;
var
  Len: Byte;
  Buf: array of Byte;
begin
  Result := '';

  Len := PByte(P)^;
  if Len <> 0 then
  begin
    SetLength(Buf, Len+1);
    Move(PByte(P)^, Buf[0], Len+1);
    Result := UTF8ToString(Buf);
  end;
end;
{$ENDIF}

function ReadShortString(var P: PByte): string;
var
  len: Integer;
begin
  {$IFDEF DELPHIXE3_UP}
  Result := _UTF8ToString(P);
  {$ELSE}
  Result := UTF8ToString(PShortString(P)^);
  {$ENDIF}
  len := ReadU8(P);
  Inc(P, len);
end;

procedure TRealPackagePatch.MakeTypeLookupTable;
  function GetUnits: TArray<string>;
  var
    p: PByte;
    i: Integer;
  begin
    SetLength(Result, FTypeInfo^.UnitCount);
    p := Pointer(FTypeInfo^.UnitNames);
    for i := 0 to FTypeInfo^.UnitCount - 1 do
      Result[i] := ReadShortString(p);
  end;

  procedure DoMake;
  var
    units: TArray<string>;
    typeIter: PPTypeInfo;
    currUnit: Integer;
    typeName: string;
    i: Integer;
    nameToType: TDictionary<string,PTypeInfo>;
  begin
    {$IFDEF DELPHIXE7_UP}
    TMonitor.Enter(Flock);
    {$ELSE}
    FLock.Acquire;
    {$ENDIF}
    try
      if FNameToType <> nil then
        Exit;

      units := GetUnits;
      currUnit := 0;
      nameToType := nil;
      try
        nameToType := TDictionary<string,PTypeInfo>.Create;
        FTypeToName := TDictionary<PTypeInfo,string>.Create;
        for i := 0 to FTypeInfo^.TypeCount - 1 do
        begin
          typeIter := FTypeInfo^.TypeTable^[i];
          if typeIter = nil then
            Continue;
          if IntPtr(typeIter) = 1 then
          begin
            Inc(currUnit);
            Continue;
          end;
          if typeIter^ = nil then
            Continue;
          {$IFDEF DELPHIXE3_UP}
          typeName := units[currUnit] + '.' + typeIter^^.NameFld.ToString;
          {$ELSE}
          typeName := units[currUnit] + '.' + UTF8ToString(typeIter^^.Name);
          {$ENDIF}

          if not nameToType.ContainsKey(typeName) then
            nameToType.Add(typeName, typeIter^);
          if not FTypeToName.ContainsKey(typeIter^) then
            FTypeToName.Add(typeIter^, typeName);
        end;
      except
        nameToType.Free;
        FreeAndNil(FTypeToName);
        raise;
      end;

      FNameToType := nameToType;
    finally
      {$IFDEF DELPHIXE7_UP}
      TMonitor.Exit(Flock);
      {$ELSE}
      FLock.Release;
      {$ENDIF}
    end;
  end;

begin
  if FNameToType <> nil then
    Exit;
  DoMake;
end;

procedure RedirectFunction(OrgProc, NewProc: Pointer);
type
  TJmpBuffer = packed record
    {$IFDEF CPUX86}
    Jmp: Byte;
    Offset: Integer;
    {$ELSE}
    MovR10_49: Byte;
    MovR10_BA: Byte;
    Target: Pointer;
    JmpR10_49: Byte;
    JmpR10_FF: Byte;
    JmpR10_E2: Byte;
    {$ENDIF}
  end;
var
  n: UINT_PTR;
  JmpBuffer: TJmpBuffer;
begin
  {$IFDEF CPUX86}
  JmpBuffer.Jmp := $E9;
  JmpBuffer.Offset := PByte(NewProc) - PByte(OrgProc) - 5;
  {$ELSE}
  JmpBuffer.MovR10_49 := $49;
  JmpBuffer.MovR10_BA := $BA;
  JmpBuffer.Target := NewProc;
  JmpBuffer.JmpR10_49 := $49;
  JmpBuffer.JmpR10_FF := $FF;
  JmpBuffer.JmpR10_E2 := $E2;
  {$ENDIF}
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @JmpBuffer, SizeOf(JmpBuffer), n) then
    RaiseLastOSError;
end;

procedure ApplyPatch;
const
  {$IFDEF DELPHIXE}
  FindTypeBytes: array[0..15] of SmallInt = (
    $55,           // push ebp
    $8B, $EC,      // mov ebp,esp
    $83, $C4, $F4, // add esp,-$0c
    $53,           // push ebx
    $8B, $DA,      // mov ebx,edx
    $89, $45, $FC, // mov [ebp-$04],eax
    $8B, $45, $FC, // mov eax,[ebp-$04]
    $E8            // call TRealPackage.MakeTypeLookupTable
  );
  {$ELSE}
  {$IFDEF CPUX86}
  FindTypeBytes: array[0..15] of SmallInt = (
    $55,           // push ebp
    $8B, $EC,      // mov ebp,esp
    $83, $C4, $F0, // add esp,-$10
    $53,           // push ebx
    $8B, $DA,      // mov ebx,edx
    $89, $45, $FC, // mov [ebp-$04],eax
    $8B, $45, $FC, // mov eax,[ebp-$04]
    $E8            // call TRealPackage.MakeTypeLookupTable
  );
  {$ELSE}
  {$IFDEF DELPHIXE2}
  FindTypeBytes: array[0..25] of SmallInt = (
    $55,                // push rbp
    $53,                // push rbx
    $48, $83, $EC, $58, // sub rsp,$58
    $48, $8B, $EC,      // mov rbp,rsp
    $48, $89, $6D, $28, // mov [rbp+$28],rbp
    $48, $89, $4D, $70, // mov [rbp+$70],rcx
    $48, $89, $55, $78, // mov [rbp+$78],rdx
    $48, $8B, $4D, $70, // mov rcx,[rbp+$70]
    $E8                 // call TRealPackage.MakeTypeLookupTable
  );
  {$ELSE}
  FindTypeBytes: array[0..24] of SmallInt = (
    $55,                // push rbp
    $48, $83, $EC, $50, // sub rsp,$50
    $48, $8B, $EC,      // mov rbp,rsp
    $48, $89, $6D, $28, // mov [rbp+$28],rbp
    $48, $89, $4D, $60, // mov [rbp+$60],rcx
    $48, $89, $55, $68, // mov [rbp+$68],rdx
    $48, $8B, $4D, $60, // mov rcx,[rbp+$60]
    $E8                 // call TRealPackage.MakeTypeLookupTable
  );
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
var
  ctx: TRttiContext;
  package: TRttiPackage;
  findType: function (const AQualifiedName: string): TRttiType of object;
  p: Pointer;
  offset: Integer;
  addr: Pointer;
begin
  package := ctx.GetPackages()[0];
  findType := package.FindType;
  p := TMethod(findType).Code;
  offset := PInteger(PByte(p) + Length(FindTypeBytes))^;
  addr := PByte(p) + Length(FindTypeBytes) + SizeOf(offset) + offset;
  RedirectFunction(addr, @TRealPackagePatch.MakeTypeLookupTable);
end;

initialization
  ApplyPatch;
{$ENDIF}{$ENDIF}

end.
