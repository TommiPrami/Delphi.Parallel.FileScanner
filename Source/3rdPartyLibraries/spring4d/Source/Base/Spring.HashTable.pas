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

unit Spring.HashTable;

interface

uses
  Generics.Defaults,
  TypInfo,
  Spring;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TEqualsMethod = function(self: Pointer; const left, right): Boolean;
  TGetHashCodeMethod = function(self: Pointer; const value): Integer;
  TEqualsMethod<T> = function(self: Pointer; const left, right: T): Boolean;
  TGetHashCodeMethod<T> = function(self: Pointer; const value: T): Integer;

  THashTableEntry = record
    HashCode, BucketIndex, ItemIndex: Integer;
  end;

  TItem<TKey> = packed record
    HashCode: Integer;
    Key: TKey;
  end;

  /// <summary>
  ///   Internally used interface to achieve virtual method dispatch on
  ///   FindWithComparer or FindWithoutComparer
  /// </summary>
  IHashTable<T> = interface
    function Find(const key: T; options: Byte = 0): Pointer;
  end;

  PHashTable = ^THashTable;
  THashTable = record
  private
    // memory layout needs to be exactly like this
    // by letting vTable point at itself we can skip
    // the 3 IInterface methods that are not needed but just the
    // Find method from IHashTable<T> to achieve the get virtual dispatch
    vTable: Pointer;
    fBuckets: TArray<Integer>;
    fItems: PByte;              // TArray<TItem>;
    fFind: Pointer;             // pointer to Find method
    fItemSize: Word;            // SizeOf(TItem)
    fDefaultComparer: Boolean;
    fOwnerships: TDictionaryOwnerships;
    fCount: Integer;
    fItemCount: Integer;
    fVersion: Integer;

    fComparer: IInterface;
    fEquals: TEqualsMethod;
    fGetHashCode: TGetHashCodeMethod;
    fItemsInfo: PTypeInfo;      // TypeInfo(TArray<TItem>)

    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetItemsInfo(const Value: PTypeInfo);
  public
  {$IFDEF DEBUG}
    class var CollisionCount: Integer;
  {$ENDIF}
    procedure Initialize(const equals, getHashCode: Pointer; typeInfo: PTypeInfo);

    procedure EnsureCompact;
    procedure Grow;
    procedure Pack;
    function FindItem(const key; options: Byte = 0): Pointer;
    function FindEntry(const key; var entry: THashTableEntry): Boolean;
    procedure Rehash(newCapacity: NativeInt);

    function DeleteEntry(const entry: THashTableEntry): Pointer;
    procedure Clear;
    procedure ClearCount;

    property Buckets: TArray<Integer> read fBuckets;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Comparer: IInterface read fComparer write fComparer;
    property Count: Integer read fCount;
    property DefaultComparer: Boolean read fDefaultComparer write fDefaultComparer;
    property Find: Pointer read fFind write fFind;
    property ItemCount: Integer read fItemCount;
    property Items: PByte read fItems;
    property ItemsInfo: PTypeInfo read fItemsInfo write SetItemsInfo;
    property ItemSize: Word read fItemSize;
    property Ownerships: TDictionaryOwnerships read fOwnerships write fOwnerships;
    property Version: Integer read fVersion;
  end;

  THashTable<T> = record
  private
    vTable: Pointer;
    fBuckets: TArray<Integer>;
    fItems: PByte;              // TArray<TItem>;
    fFind: Pointer;
    fItemSize: Word;            // SizeOf(TItem)
    fDefaultComparer: Boolean;
    fOwnerships: TDictionaryOwnerships;
    fCount: Integer;
    fItemCount: Integer;
    fVersion: Integer;

    fComparer: IEqualityComparer<T>;
    fEquals: TEqualsMethod;
    fGetHashCode: TGetHashCodeMethod;
    fItemsInfo: PTypeInfo;      // TypeInfo(TArray<TItem>)

    {$IFDEF DELPHIXE7_UP}
    class procedure __SuppressWarning(var value); static; inline;
    {$ENDIF}
  public
    function FindWithComparer(const key: T; options: Byte): Pointer;
    {$IFDEF DELPHIXE7_UP}
    function FindWithoutComparer(const key: T; options: Byte): Pointer;
    {$ENDIF}

    property Buckets: TArray<Integer> read fBuckets;
    property Comparer: IEqualityComparer<T> read fComparer write fComparer;
    property Count: Integer read fCount;
    property DefaultComparer: Boolean read fDefaultComparer write fDefaultComparer;
    property Equals: TEqualsMethod read fEquals;
    property Find: Pointer read fFind write fFind;
    property GetHashCode: TGetHashCodeMethod read fGetHashCode;
    property ItemCount: Integer read fItemCount;
    property Items: PByte read fItems;
    property ItemsInfo: PTypeInfo read fItemsInfo;
    property ItemSize: Word read fItemSize;
    property Ownerships: TDictionaryOwnerships read fOwnerships write fOwnerships;
    property Version: Integer read fVersion;
  end;

  TStackData = record
    hashTable: PHashTable;
    hashCode, perturb, bucketIndex: Integer;
  end;

const
  KeyOffset = SizeOf(Integer);

  // use the MSB of the HashCode to note removed items
  RemovedFlag        = Integer($80000000);
  MinCapacity        = 6; // 75% load factor leads to min bucket count of 8
  MaxCapacity        = $30000000; // 75% of highest 2^n Integer
  PerturbShift       = 5;
  EmptyBucket        = -1; // must be negative, note choice of BucketSentinelFlag
  UsedBucket         = -2; // likewise

  IgnoreExisting     = 1;
  OverwriteExisting  = 2;
  RaiseOnExisting    = 4;
  DeleteExisting     = 8;
  InsertNonExisting  = 16;
  MarkNonExisting    = 32;
  RaiseOnNonExisting = 64;
  ExistingMask       = IgnoreExisting or OverwriteExisting or RaiseOnExisting or DeleteExisting;
  MarkNonExistingShift = 32-6; // number of bits that the MarkNonExisting bit needs to be shifted left to result RemovedFlag

{$IF defined(DELPHIXE6) or defined(DELPHIXE7)}
function HashTableFindItem(hashTable: PHashTable; const key; options: Byte = 0): Pointer;
{$IFEND}

implementation

uses
  SysUtils,
  Spring.Comparers,
  Spring.Hash,
  Spring.ResourceStrings;

{$IF defined(DELPHIXE6) or defined(DELPHIXE7)}
  // there seems to be some compiler glitch in XE6 and XE7 that causes
  // internal compiler error in some rare cases when using goto - here is one
  {$DEFINE GOTO_OFF}

function HashTableFindItem(hashTable: PHashTable; const key; options: Byte = 0): Pointer;
begin
  Result := hashTable.FindItem(key, options);
end;
{$IFEND}

// copied here from Spring.pas to enable inlining
function DynArrayLength(const A: Pointer): NativeInt; inline;
begin
  Result := NativeInt(A);
  if Result <> 0 then
    {$POINTERMATH ON}
    Result := PNativeInt(Result)[-1];
    {$POINTERMATH OFF}
end;

procedure __SuppressWarning(var value); inline;
begin
end;

function GetMarkNonExistingMask(const options: Byte): Integer; inline;
begin
  Result := Integer(options) and MarkNonExisting shl MarkNonExistingShift;
end;


{$REGION 'THashTable'}

procedure THashTable.Initialize(const equals, getHashCode: Pointer; typeInfo: PTypeInfo);
var
  comparer: Pointer;
begin
  vTable := @vTable;
  fEquals := equals;
  fGetHashCode := getHashCode;
  comparer := _LookupVtableInfo(giEqualityComparer, typeInfo, GetTypeSize(typeInfo));
  if not Assigned(fComparer) then
  begin
    Pointer(fComparer) := comparer;
    fComparer._AddRef;
  end;
  fDefaultComparer := Pointer(fComparer) = comparer;
  if PassByRef(typeInfo, ccReg, True) then
  begin
    fEquals := PPVTable(fComparer)^[3];
    fGetHashCode := PPVTable(fComparer)^[4];
  end;
end;

function THashTable.GetCapacity: Integer;
begin
  Result := Integer(DynArrayLength(fItems));
end;

function THashTable.DeleteEntry(const entry: THashTableEntry): Pointer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  fBuckets[entry.BucketIndex] := UsedBucket;
  Dec(fCount);

  Result := @fItems[NativeInt(entry.ItemIndex) * fItemSize];
  PInteger(Result)^ := RemovedFlag;
end;

procedure THashTable.Clear;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCount := 0;
  fItemCount := 0;
  fBuckets := nil;
  DynArrayClear(Pointer(fItems), fItemsInfo);
  Capacity := 0;
end;

procedure THashTable.ClearCount;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCount := 0;
  fItemCount := 0;
end;

procedure THashTable.EnsureCompact;
begin
  if fCount <> fItemCount then
    Rehash(Capacity);
end;

procedure THashTable.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Capacity;
  if newCapacity = 0 then
    newCapacity := MinCapacity
  else if 2 * fCount >= DynArrayLength(fBuckets) then
    // only grow if load factor is greater than 0.5
    newCapacity := newCapacity * 2;
  if Cardinal(newCapacity) > MaxCapacity then
    SysUtils.OutOfMemoryError;
  Rehash(newCapacity);
end;

procedure THashTable.Pack;
var
  sourceItem, targetItem: PByte;
  itemType: PTypeInfo;
  i: Integer;
begin
  sourceItem := fItems;
  targetItem := fItems;
  itemType := fItemsInfo.TypeData.elType2^;
  for i := 0 to fItemCount - 1 do //FI:W528
  begin
    if PInteger(sourceItem)^ >= 0 then // not removed
    begin
      if targetItem < sourceItem then // need to fill a gap caused by previous items that were removed
        CopyRecord(targetItem, sourceItem, itemType);
      Inc(targetItem, fItemSize);
    end;
    Inc(sourceItem, fItemSize);
  end;
  FinalizeArray(targetItem, itemType, Cardinal(fItemCount - fCount)); // clear remaining items that were previously moved
end;

function THashTable.FindItem(const key; options: Byte): Pointer;
label
  loopStart, notFound, findAgain;
var
  hashCode, bucketIndex, itemIndex, perturb, mask: Integer;
  hashTable: PHashTable;
  item: PByte;
begin
  hashTable := @vTable;

  hashCode := hashTable.fGetHashCode(Pointer(hashTable.fComparer), key) and not RemovedFlag;

  if hashTable.Buckets = nil then goto notFound;

findAgain:
  perturb := hashCode;
  mask := PInteger(@PByte(hashTable.Buckets)[-SizeOf(NativeInt)])^ - 1;
  bucketIndex := hashCode and mask;
  goto loopStart;

  repeat
    // lookup Item and deal with collisions
    repeat
    {$IFDEF DEBUG}
      Inc(CollisionCount);
    {$ENDIF}

      perturb := perturb shr PerturbShift;
      bucketIndex := (5 * bucketIndex + 1 + perturb) and mask;

    loopStart:
      itemIndex := hashTable.Buckets[bucketIndex];
      if itemIndex = UsedBucket then Continue;
      if itemIndex = EmptyBucket then goto notFound;
      if (itemIndex xor hashCode) and not mask = 0 then Break;
    until False;

    item := @hashTable.Items[NativeInt(hashTable.ItemSize) * (itemIndex and mask)];

    if not hashTable.fEquals(Pointer(hashTable.fComparer), item[KeyOffset], key) then Continue;

    if options and ExistingMask <> 0 then
      if options and IgnoreExisting <> 0 then
        Exit(nil)
      else if options and OverwriteExisting <> 0 then
      begin
        {$Q-}
        Inc(hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        // mark hashCode to indicate overwriting - caller needs to unset the bit again!
        PInteger(item)^ := PInteger(item)^ or RemovedFlag;
      end
      else if options and DeleteExisting <> 0 then
      begin
        {$Q-}
        Inc(hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        hashTable.Buckets[bucketIndex] := UsedBucket;
        Dec(hashTable.fCount);

        PInteger(item)^ := RemovedFlag;
      end
      else
      begin
        RaiseHelper.DuplicateKey;
        Exit(nil);
      end;
    Exit(item);

  notFound:
    if options and RaiseOnNonExisting = 0 then
    begin
      if options and InsertNonExisting <> 0 then
      begin
        if Assigned(hashTable.fItems) then
        if hashTable.ItemCount < PInteger(@PByte(hashTable.Items)[-SizeOf(NativeInt)])^ then
        begin
          {$Q-}
          Inc(hashTable.fVersion);
          {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

          itemIndex := hashTable.ItemCount;
          hashTable.Buckets[bucketIndex] := itemIndex or (hashCode and not mask);
          Inc(hashTable.fCount);
          Inc(hashTable.fItemCount);

          Result := @hashTable.Items[NativeInt(hashTable.ItemSize) * itemIndex];
          PInteger(Result)^ := hashCode or GetMarkNonExistingMask(options);
          Exit;
        end;
        hashTable.Grow;
        goto findAgain;
      end;
      Exit(nil);
    end;
    RaiseHelper.KeyNotFound;
    Exit(nil);
  until False;
  __SuppressWarning(bucketIndex);
  __SuppressWarning(mask);
end;

function THashTable.FindEntry(const key; var entry: THashTableEntry): Boolean;
{$IFNDEF GOTO_OFF}
label
  first;
{$ENDIF}
var
  perturb, mask: Integer;
  hashTable: PHashTable;
  item: PByte;
begin
  hashTable := @vTable;
  if hashTable.Buckets <> nil then
  begin
    entry.HashCode := entry.HashCode and not RemovedFlag;
    perturb := entry.HashCode;
    mask := PInteger(@PByte(hashTable.Buckets)[-SizeOf(NativeInt)])^ - 1;
    entry.BucketIndex := entry.HashCode and mask;

    {$IFNDEF GOTO_OFF}
    goto first;
    repeat
    {$IFDEF DEBUG}
      Inc(CollisionCount);
    {$ENDIF}

      perturb := perturb shr PerturbShift;
      entry.BucketIndex := (5 * entry.BucketIndex + 1 + perturb) and mask;

    first:
      entry.ItemIndex := hashTable.Buckets[entry.BucketIndex];
      if entry.ItemIndex = UsedBucket then Continue;
      if entry.ItemIndex = EmptyBucket then Break;
      if (entry.ItemIndex xor entry.HashCode) and not mask = 0 then
      begin
        entry.ItemIndex := entry.ItemIndex and mask;
        item := @hashTable.Items[NativeInt(hashTable.ItemSize) * entry.ItemIndex];
        Result := hashTable.fEquals(Pointer(hashTable.fComparer), item[KeyOffset], key);
        if Result then Exit;
      end;
    until False;
    {$ELSE}
    repeat
      entry.ItemIndex := hashTable.Buckets[entry.BucketIndex];
      if entry.ItemIndex <> UsedBucket then
      begin
        if entry.ItemIndex = EmptyBucket then Break;
        if (entry.ItemIndex xor entry.HashCode) and not mask = 0 then
        begin
          entry.ItemIndex := entry.ItemIndex and mask;
          item := @hashTable.Items[NativeInt(hashTable.ItemSize) * entry.ItemIndex];
          Result := hashTable.fEquals(Pointer(hashTable.fComparer), item[KeyOffset], key);
          if Result then Exit;
        end;
      end;

    {$IFDEF DEBUG}
      Inc(CollisionCount);
    {$ENDIF}

      perturb := perturb shr PerturbShift;
      entry.BucketIndex := (5 * entry.BucketIndex + 1 + perturb) and mask;
    until False;
    {$ENDIF}
  end;
  entry.ItemIndex := hashTable.ItemCount;
  Result := False;
end;

procedure THashTable.Rehash(newCapacity: NativeInt);
var
  newBucketCount: NativeInt;
  mask: Integer;
  entry: THashTableEntry;
  item: PByte;
begin
  if newCapacity = 0 then
  begin
    Assert(fCount = 0);
    Assert(fItemCount = 0);
    Assert(not Assigned(fBuckets));
    Assert(not Assigned(fItems));
    Exit;
  end;

  Assert(newCapacity >= fCount);

  newBucketCount := NextPowerOf2(NativeInt(NativeUInt(newCapacity) * 4 div 3 - 1)); // 75% load factor
  newCapacity := NativeInt(NativeUInt(newBucketCount) * 3 div 4);

  if (newCapacity = Capacity) and (fItemCount = fCount) then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  Pack;

  if newCapacity <> Capacity then
  begin
    // resize the items array, safe now that we have compacted it
    DynArraySetLength(Pointer(fItems), fItemsInfo, 1, @newCapacity);
    Assert(Capacity >= fCount);

    // repopulate the bucket array
    SetLength(fBuckets, newBucketCount);
  end;
  FillChar(fBuckets[0], newBucketCount * SizeOf(Integer), $FF);

  item := fItems;
  mask := PInteger(@PByte(Buckets)[-SizeOf(NativeInt)])^ - 1;
  fItemCount := 0;
  while fItemCount < fCount do
  begin
    entry.HashCode := PInteger(item)^;
    FindEntry((item + KeyOffset)^, entry);
    fBuckets[entry.BucketIndex] := fItemCount or (entry.HashCode and not mask);
    Inc(item, fItemSize);
    Inc(fItemCount);
  end;
end;

procedure THashTable.SetCapacity(const value: Integer);
var
  newCapacity: Integer;
begin
  if value <= MaxCapacity then
    if value >= fCount then
    begin
      newCapacity := value;
      if (newCapacity > 0) and (newCapacity < MinCapacity) then
        newCapacity := MinCapacity;
      if newCapacity <> DynArrayLength(fItems) then
        Rehash(newCapacity);
    end
    else
      RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity)
  else
    SysUtils.OutOfMemoryError;
end;

procedure THashTable.SetItemsInfo(const value: PTypeInfo);
begin
  fItemsInfo := value;
  fItemSize := Word(value.TypeData.elSize);
end;

{$ENDREGION}


{$REGION 'THashTable<T>'}

{$IFDEF DELPHIXE7_UP}
class procedure THashTable<T>.__SuppressWarning(var value);
begin
end;
{$ENDIF}

function THashTable<T>.FindWithComparer(const key: T; options: Byte): Pointer;
label
  loopStart, notFound, deletedFound, findAgain;
var
  hashCode, bucketIndex, itemIndex, perturb, mask: Integer;
  hashTable: PHashTable;
  item: Pointer;
  stackData: TStackData;
begin
  stackData.hashTable := @vTable;

  hashCode := THashTable<T>(stackData.hashTable^).Comparer.GetHashCode(key);
  hashCode := hashCode and not RemovedFlag;

  stackData.hashCode := hashCode;
  if stackData.hashTable.Buckets = nil then goto notFound;

findAgain:
  hashCode := stackData.hashCode;
  stackData.perturb := hashCode;
  stackData.bucketIndex := -1;
  mask := PInteger(@PByte(stackData.hashTable.Buckets)[-SizeOf(NativeInt)])^ - 1;
  bucketIndex := hashCode and mask;
  goto loopStart;

deletedFound:
  if options and InsertNonExisting <> 0 then
    if stackData.bucketIndex < 0 then
      stackData.bucketIndex := bucketIndex;

  repeat
    // lookup Item and deal with collisions
    repeat
//    {$IFDEF DEBUG}
//      Inc(THashTable.CollisionCount);
//    {$ENDIF}

      perturb := stackData.perturb;
      perturb := perturb shr PerturbShift;
      bucketIndex := (5 * bucketIndex + 1 + perturb) and mask;
      stackData.perturb := perturb;

    loopStart:
      hashTable := stackData.hashTable;
      itemIndex := hashTable.Buckets[bucketIndex];
      if itemIndex = UsedBucket then goto deletedFound;
      if itemIndex = EmptyBucket then goto notFound;
      if (itemIndex xor hashCode) and not mask = 0 then Break;
    until False;

    item := @hashTable.Items[NativeInt(hashTable.ItemSize) * (itemIndex and mask)];

    if not THashTable<T>(stackData.hashTable^).Comparer.Equals(key, TItem<T>(item^).Key) then Continue;

    if options and ExistingMask <> 0 then
      if options and IgnoreExisting <> 0 then
        Exit(nil)
      else if options and OverwriteExisting <> 0 then
      begin
        {$Q-}
        Inc(stackData.hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        // mark hashCode to indicate overwriting - caller needs to unset the bit again!
        PInteger(item)^ := PInteger(item)^ or RemovedFlag;
      end
      else if options and DeleteExisting <> 0 then
      begin
        {$Q-}
        Inc(stackData.hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        stackData.hashTable.Buckets[bucketIndex] := UsedBucket;
        Dec(stackData.hashTable.fCount);

        PInteger(item)^ := RemovedFlag;
      end
      else
      begin
        RaiseHelper.DuplicateKey;
        Exit(nil);
      end;
    Exit(item);

  notFound:
    if options and RaiseOnNonExisting = 0 then
    begin
      if options and InsertNonExisting <> 0 then
      begin
        hashTable := stackData.hashTable;
        if Assigned(hashTable.fItems) then
        if hashTable.ItemCount < PInteger(@PByte(hashTable.Items)[-SizeOf(NativeInt)])^ then
        begin
          {$Q-}
          Inc(hashTable.fVersion);
          {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

          itemIndex := hashTable.ItemCount;
          if stackData.bucketIndex >= 0 then
            bucketIndex := stackData.bucketIndex;
          hashTable.Buckets[bucketIndex] := itemIndex or (hashCode and not mask);
          Inc(hashTable.fCount);
          Inc(hashTable.fItemCount);

          Result := @hashTable.Items[NativeInt(hashTable.ItemSize) * itemIndex];
          PInteger(Result)^ := stackData.hashCode or GetMarkNonExistingMask(options);
          Exit;
        end;
        stackData.hashTable.Grow;
        goto findAgain;
      end;
      Exit(nil);
    end;
    RaiseHelper.KeyNotFound;
    Exit(nil);
  until False;
  {$IFDEF DELPHIXE7_UP}
  __SuppressWarning(bucketIndex);
  __SuppressWarning(mask);
  {$ENDIF}
end;

{$IFDEF DELPHIXE7_UP}
function THashTable<T>.FindWithoutComparer(const key: T; options: Byte): Pointer;
type
  PInt64Rec = ^Int64Rec;
label
  loopStart, notFound, deletedFound, findAgain;
var
  hashCode, bucketIndex, itemIndex, perturb, mask: Integer;
  hashTable: PHashTable;
  item: Pointer;
  stackData: TStackData;
begin
  stackData.hashTable := @vTable;

  // GetHashCode
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkWChar, tkInterface, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: hashCode := PShortInt(@key)^ and not RemovedFlag;
        2: hashCode := PSmallInt(@key)^ and not RemovedFlag;
        4: hashCode := PInteger(@key)^ and not RemovedFlag;
        8: hashCode := Integer(PInt64Rec(@key).Cardinals[0] xor PInt64Rec(@key).Cardinals[1]) and not RemovedFlag;
      end;
    tkLString:
    begin
      hashCode := 0;
      if PPointer(@key)^ <> nil then
      {$R-}
        hashCode := DefaultHashFunction(PPointer(@key)^^, PCardinal(PByte((@key)^) - 4)^) and not RemovedFlag;
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
    end;
    tkUString:
    begin
      hashCode := 0;
      if PPointer(@key)^ <> nil then
      {$R-}
        hashCode := DefaultHashFunction(PPointer(@key)^^, PCardinal(PByte((@key)^) - 4)^ * SizeOf(Char)) and not RemovedFlag;
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
    end;
    tkRecord:
      if TypeInfo(T) = TypeInfo(TGUID) then
        hashCode := DefaultHashFunction(key, SizeOf(TGUID)) and not RemovedFlag
      else
        hashCode := THashTable<T>(stackData.hashTable^).Comparer.GetHashCode(key) and not RemovedFlag;
  else
    hashCode := THashTable<T>(stackData.hashTable^).Comparer.GetHashCode(key) and not RemovedFlag;
  end;

  if stackData.hashTable.Buckets = nil then goto notFound;

findAgain:
  stackData.perturb := hashCode;
  stackData.bucketIndex := -1;
  mask := PInteger(@PByte(stackData.hashTable.Buckets)[-SizeOf(NativeInt)])^ - 1;
  bucketIndex := hashCode and mask;
  goto loopStart;

deletedFound:
  if options and InsertNonExisting <> 0 then
    if stackData.bucketIndex < 0 then
      stackData.bucketIndex := bucketIndex;

  repeat
    // lookup Item and deal with collisions
    repeat
//    {$IFDEF DEBUG}
//      Inc(THashTable.CollisionCount);
//    {$ENDIF}

      perturb := stackData.perturb;
      perturb := perturb shr PerturbShift;
      bucketIndex := (5 * bucketIndex + 1 + perturb) and mask;
      stackData.perturb := perturb;

    loopStart:
      hashTable := stackData.hashTable;
      itemIndex := hashTable.Buckets[bucketIndex];
      if itemIndex = UsedBucket then goto deletedFound;
      if itemIndex = EmptyBucket then goto notFound;
      if (itemIndex xor hashCode) and not mask = 0 then Break;
    until False;

    item := @hashTable.Items[NativeInt(hashTable.ItemSize) * (NativeInt(itemIndex) and mask)];

    // Equals(item.Key, key)
    case GetTypeKind(T) of
      tkInteger, tkChar, tkEnumeration, tkWChar, tkInterface, tkInt64, tkClassRef, tkPointer, tkProcedure:
        case SizeOf(T) of
          1: if TItem<Byte>(item^).Key <> PByte(@key)^ then Continue;
          2: if TItem<Word>(item^).Key <> PWord(@key)^ then Continue;
          4: if TItem<Cardinal>(item^).Key <> PCardinal(@key)^ then Continue;
          {$IFDEF CPU32BITS}
          8: if ((TItem<Int64Rec>(item^).Key.Lo xor PInt64Rec(@key).Cardinals[0])
            or (TItem<Int64Rec>(item^).Key.Hi xor PInt64Rec(@key).Cardinals[1])) <> 0 then Continue;
          {$ELSE}
          8: if TItem<Int64>(item^).Key <> PInt64(@key)^ then Continue;
          {$ENDIF}
        end;
      tkLString:
        if PAnsiString(@key)^ <> TItem<AnsiString>(item^).Key then Continue;
      tkUString:
        if PString(@key)^ <> TItem<string>(item^).Key then Continue;
      tkRecord:
        if TypeInfo(T) = TypeInfo(TGUID) then
          if not SameGuid(TItem<TGUID>(item^).Key, PGUID(@key)^) then Continue else //FI:W506
        else
          if not THashTable<T>(stackData.hashTable^).Comparer.Equals(TItem<T>(item^).Key, key) then Continue;
    else
      if not THashTable<T>(stackData.hashTable^).Comparer.Equals(TItem<T>(item^).Key, key) then Continue;
    end;

    if options and ExistingMask <> 0 then
      if options and IgnoreExisting <> 0 then
        Exit(nil)
      else if options and OverwriteExisting <> 0 then
      begin
        {$Q-}
        Inc(stackData.hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        // mark hashCode to indicate overwriting - caller needs to unset the bit again!
        PInteger(item)^ := PInteger(item)^ or RemovedFlag;
      end
      else if options and DeleteExisting <> 0 then
      begin
        {$Q-}
        Inc(stackData.hashTable.fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

        stackData.hashTable.Buckets[bucketIndex] := UsedBucket;
        Dec(stackData.hashTable.fCount);

        PInteger(item)^ := RemovedFlag;
      end
      else
      begin
        RaiseHelper.DuplicateKey;
        Exit(nil);
      end;
    Exit(item);

  notFound:
    if options and RaiseOnNonExisting = 0 then
    begin
      if options and InsertNonExisting <> 0 then
      begin
        hashTable := stackData.hashTable;
        if Assigned(hashTable.fItems) then
        if hashTable.fItemCount < PInteger(@PByte(hashTable.fItems)[-SizeOf(NativeInt)])^ then
        begin
          {$Q-}
          Inc(hashTable.fVersion);
          {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

          itemIndex := hashTable.fItemCount;
          if stackData.bucketIndex >= 0 then
            bucketIndex := stackData.bucketIndex;
          hashTable.Buckets[bucketIndex] := itemIndex or (hashCode and not mask);
          Inc(hashTable.fCount);
          Inc(hashTable.fItemCount);

          Result := @hashTable.Items[NativeInt(hashTable.ItemSize) * itemIndex];
          PInteger(Result)^ := hashCode or GetMarkNonExistingMask(options);
          Exit;
        end;
        stackData.hashTable.Grow;
        goto findAgain;
      end;
      Exit(nil);
    end;
    RaiseHelper.KeyNotFound;
    Exit(nil);
  until False;
  __SuppressWarning(bucketIndex);
  __SuppressWarning(mask);
  __SuppressWarning(hashCode);
end;
{$ENDIF}

{$ENDREGION}


end.
