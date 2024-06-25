{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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

unit Spring.Span;

interface

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  Span<T> = packed record
  public type
    PT = ^T;
  private type
    TEnumerator = record
    private
      fCurrent, fEnd: PT;
    public
      function MoveNext: Boolean; inline;
      property Current: PT read fCurrent;
    end;
  private
    fData: Pointer;
    fLength: NativeInt;
    function GetIsEmpty: Boolean; inline;
    function GetItem(index: NativeInt): PT; inline;
  public
    class function Create(const arr: TArray<T>): Span<T>; overload; static; inline;
    class function Create(const arr: TArray<T>; startIndex: NativeInt): Span<T>; overload; static; inline;
    class function Create(const arr: TArray<T>; startIndex, length: NativeInt): Span<T>; overload; static; inline;
    class function Create(const p: Pointer; length: NativeInt): Span<T>; overload; static; inline;

    procedure Init(const p: Pointer; length: NativeInt); inline;

    function GetEnumerator: TEnumerator;

    function Slice(startIndex: NativeInt): Span<T>; overload; inline;
    function Slice(startIndex, length: NativeInt): Span<T>; overload; inline;

    function ToArray: TArray<T>;

    property Data: Pointer read fData;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[index: NativeInt]: PT read GetItem; default;
    property Length: NativeInt read fLength;

    class operator Implicit(const arr: TArray<T>): Span<T>;
    class operator Equal(const left, right: Span<T>): Boolean; inline;
    class operator NotEqual(const left, right: Span<T>): Boolean; inline;
  end;

implementation

{$IFNDEF DELPHIXE7_UP}
uses
  Rtti;
{$ENDIF}

{$Q-}
{$R-}
function DynArrayLength(const A: Pointer): NativeInt; inline;
begin
  Result := NativeInt(A);
  if Result <> 0 then
    {$POINTERMATH ON}
    Result := PNativeInt(Result)[-1];
    {$POINTERMATH OFF}
end;
{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
{$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}


{$REGION 'Span<T>'}

class function Span<T>.Create(const arr: TArray<T>): Span<T>;
begin
  Result.fData := Pointer(arr);
  Result.fLength := DynArrayLength(arr);
end;

class function Span<T>.Create(const arr: TArray<T>; startIndex: NativeInt): Span<T>;
begin
  Result.fData := @arr[startIndex];
  Result.fLength := DynArrayLength(arr) - startIndex;
end;

class function Span<T>.Create(const arr: TArray<T>; startIndex, length: NativeInt): Span<T>;
begin
  Result.fData := @arr[startIndex];
  Result.fLength := length;
end;

class function Span<T>.Create(const p: Pointer; length: NativeInt): Span<T>;
begin
  Result.fData := p;
  Result.fLength := length;
end;

procedure Span<T>.Init(const p: Pointer; length: NativeInt);
begin
  fData := p;
  fLength := length;
end;

function Span<T>.GetEnumerator: TEnumerator;
var
  data: PByte;
begin
  data := Pointer(fData);
  Result.fEnd := Pointer(data + fLength * SizeOf(T));
  Result.fCurrent := Pointer(data - SizeOf(T));
end;

function Span<T>.GetIsEmpty: Boolean;
begin
  Result := fLength = 0;
end;

function Span<T>.GetItem(index: NativeInt): PT;
begin
  {$R-}
  Result := @TArray<T>(fData)[index];
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

function Span<T>.Slice(startIndex: NativeInt): Span<T>;
begin
  {$R-}
  Result.fData := @TArray<T>(fData)[startIndex];
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Result.fLength := fLength - startIndex;
end;

function Span<T>.Slice(startIndex, length: NativeInt): Span<T>;
begin
  {$R-}
  Result.fData := @TArray<T>(fData)[startIndex];
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Result.fLength := length;
end;

function Span<T>.ToArray: TArray<T>;
begin
  SetLength(Result, fLength);
  if {$IFDEF DELPHIXE7_UP}System.IsManagedType(T){$ELSE}Rtti.IsManaged(TypeInfo(T)){$ENDIF}then
    CopyArray(Pointer(Result), fData, TypeInfo(T), fLength)
  else
    Move(fData^, Pointer(Result)^, fLength * SizeOf(T));
end;

class operator Span<T>.Implicit(const arr: TArray<T>): Span<T>;
begin
  Result.fData := Pointer(arr);
  Result.fLength := DynArrayLength(arr);
end;

class operator Span<T>.Equal(const left, right: Span<T>): Boolean;
begin
  Result := (left.fLength = right.fLength) and (left.fData = right.fData);
end;

class operator Span<T>.NotEqual(const left, right: Span<T>): Boolean;
begin
  Result := (left.fLength <> right.fLength) or (left.fData <> right.fData);
end;

{$ENDREGION}


{$REGION 'Span<T>.TEnumerator'}

function Span<T>.TEnumerator.MoveNext: Boolean;
begin
  Inc(fCurrent);
  Result := PByte(fCurrent) < PByte(fEnd);
end;

{$ENDREGION}


end.
