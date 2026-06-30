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

unit Spring.Collections.Sets;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  THashSetItem<T> = packed record
  public
    HashCode: Integer;
    Item: T;
  end;

  /// <summary>
  ///   Represents a set of values.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the hash set.
  /// </typeparam>
  THashSet<T> = class(TCollectionBase<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>, IOrderedSet<T>)
  private type
  {$REGION 'Nested Types'}
    TItem = THashSetItem<T>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: THashSet<T>;
      fIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemByIndex(index: Integer): T;
    function GetNonEnumeratedCount: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    procedure CheckUniqueAndUnfoundElements(const other: IEnumerable<T>;
      returnIfUnfound: Boolean; var uniqueCount, unfoundCount: Integer);
    procedure IntersectWithEnumerable(const other: IEnumerable<T>);
  public
    constructor Create(elementType: PTypeInfo; capacity: Integer; const comparer: IEqualityComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
    function TryGetElementAt(var item: T; index: Integer): Boolean;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    procedure AddRange(const values: array of T); overload;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
    procedure TrimExcess;
  {$ENDREGION}

  {$REGION 'Implements IOrderedSet<T>'}
    function IndexOf(const key: T): Integer;
  {$ENDREGION}
  end;

  TSortedSet<T> = class(TCollectionBase<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>)
  private type
  {$REGION 'Nested Types'}
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<T>.TNode
      Parent, Right, Left: PNode;
      Key: T;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TSortedSet<T>;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<T>;
    fVersion: Integer;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    procedure IntersectWithEnumerable(const other: IEnumerable<T>);
  public
    constructor Create(elementType: PTypeInfo; const comparer: IComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
    procedure TrimExcess;
  {$ENDREGION}
  end;

implementation

uses
  TypInfo,
  Spring.Comparers,
  Spring.Span;

const
  StackAllocThreshold = 100;


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create(elementType: PTypeInfo; capacity: Integer; const comparer: IEqualityComparer<T>);
begin
  fElementType := elementType;
  fHashTable.Comparer := comparer;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  SetCapacity(capacity);
end;

procedure THashSet<T>.AfterConstruction;
begin
  inherited AfterConstruction;

  fHashTable.Initialize(TComparerThunks<T>.Equals, TComparerThunks<T>.GetHashCode, fElementType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<T>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<T>.FindWithComparer;
end;

procedure THashSet<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

procedure THashSet<T>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

procedure THashSet<T>.TrimExcess;
begin
  fHashTable.Capacity := fHashTable.Count;
end;

function THashSet<T>.TryGetElementAt(var item: T; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    item := TItems(fHashTable.Items)[index].Item;
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function THashSet<T>.Add(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item, IgnoreExisting or InsertNonExisting);
  if not Assigned(entry) then Exit(Boolean(Pointer(entry)));
  entry.Item := item;
  DoNotify(item, caAdded);
  Result := True;
end;

procedure THashSet<T>.AddRange(const values: array of T);
var
  i: NativeInt;
  entry: PItem;
begin
  fHashTable.Capacity := fHashTable.Count + Length(values);
  for i := 0 to High(values) do
  begin
    entry := IHashTable<T>(@fHashTable).Find(values[i], IgnoreExisting or InsertNonExisting);
    if not Assigned(entry) then Continue;
    entry.Item := values[i];
    if Assigned(Notify) then
      Notify(Self, entry.Item, caAdded);
  end;
end;

procedure THashSet<T>.CheckUniqueAndUnfoundElements(const other: IEnumerable<T>;
  returnIfUnfound: Boolean; var uniqueCount, unfoundCount: Integer);
var
  intArrayLength: Integer;
  buffer: array[0..StackAllocThreshold-1] of Integer;
  heapAlloc: Boolean;
  bitHelper: PIntegerArray;

  enumerator: IEnumerator<T>;
  item: T;
  entry: THashTableEntry;
  index: NativeUInt;
  bitMask: Integer;
begin
  uniqueCount := 0;
  if fHashTable.Count = 0 then
  begin
    unfoundCount := Ord(other.Any);
    Exit;
  end;

  intArrayLength := fHashTable.ItemCount;
  if intArrayLength > 0 then
    intArrayLength := (intArrayLength - 1) shr 5 + 1;
  if intArrayLength <= StackAllocThreshold then
  begin
    heapAlloc := False;
    bitHelper := @buffer[0];
    FillChar(bitHelper^, intArrayLength shl 2, 0);
  end
  else
  begin
    heapAlloc := True;
    bitHelper := AllocMem(intArrayLength shl 2);
  end;

  try
    unfoundCount := 0;
    enumerator := other.GetEnumerator;
    while enumerator.MoveNext do
    begin
      {$IFDEF MANAGED_TYPE_RVO_BROKEN}
      if IsManagedType(T) then
        IEnumeratorInternal(enumerator).GetCurrent(item)
      else{$ENDIF}
      item := enumerator.Current;
      entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(item);
      if fHashTable.FindEntry(item, entry) then
      begin
        index := NativeUInt(entry.ItemIndex);
        bitMask := 1 shl (Byte(index) and 31);
        index := index shr 5;
        if bitHelper[index] and bitMask = 0 then
        begin
          bitHelper[index] := bitHelper[index] or bitMask;
          Inc(uniqueCount);
        end;
      end
      else
      begin
        Inc(unfoundCount);
        if returnIfUnfound then
          Break;
      end;
    end;
  finally
    if heapAlloc then
      FreeMem(bitHelper);
  end;
end;

procedure THashSet<T>.Clear;
var
  item: PItem;
  itemCount, i: Integer;
begin
  if Assigned(Notify) then
  begin
    itemCount := fHashTable.ItemCount;
    fHashTable.ClearCount;
    if itemCount > 0 then
      Reset;
    item := PItem(fHashTable.Items);
    for i := 1 to itemCount do //FI:W528
    begin
      if item.HashCode >= 0 then
        Notify(Self, item.Item, caRemoved);
      Inc(item);
    end;
  end;

  fHashTable.Clear;
end;

function THashSet<T>.Contains(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  Result := Assigned(entry);
end;

function THashSet<T>.Extract(const item: T): T;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
  if Assigned(entry) then
  begin
    if Assigned(Notify) then
      Notify(Self, entry.Item, caExtracted);
    Result := entry.Item;
    entry.Item := Default(T);
  end
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function THashSet<T>.GetCapacity: Integer;
begin
  Result := fHashTable.Capacity;
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function THashSet<T>.GetItemByIndex(index: Integer): T; //FI:W521
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    Exit(TItems(fHashTable.Items)[index].Item);
  end;
  RaiseHelper.ArgumentOutOfRange_Index;
  __SuppressWarning(Result);
end;

function THashSet<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fHashTable.Count;
end;

function THashSet<T>.IndexOf(const key: T): Integer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(key);
  fHashTable.EnsureCompact;
  if fHashTable.FindEntry(key, entry) then
    Exit(entry.ItemIndex);
  Result := -1;
end;

procedure THashSet<T>.IntersectWithEnumerable(const other: IEnumerable<T>);
var
  originalCount, intArrayLength: Integer;
  buffer: array[0..StackAllocThreshold-1] of Integer;
  heapAlloc: Boolean;
  bitHelper: PIntegerArray;

  enumerator: IEnumerator<T>;
  current: T;
  entry: THashTableEntry;
  index, i: NativeUInt;
begin
  originalCount := fHashTable.ItemCount;
  intArrayLength := originalCount;
  if intArrayLength > 0 then
    intArrayLength := (intArrayLength - 1) shr 5 + 1;
  if intArrayLength <= StackAllocThreshold then
  begin
    heapAlloc := False;
    bitHelper := @buffer[0];
    FillChar(bitHelper^, intArrayLength shl 2, 0);
  end
  else
  begin
    heapAlloc := True;
    bitHelper := AllocMem(intArrayLength shl 2);
  end;

  try
    enumerator := other.GetEnumerator;
    while enumerator.MoveNext do
    begin
      {$IFDEF MANAGED_TYPE_RVO_BROKEN}
      if IsManagedType(T) then
        IEnumeratorInternal(enumerator).GetCurrent(current)
      else{$ENDIF}
      current := enumerator.Current;
      entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(current);
      if fHashTable.FindEntry(current, entry) then
      begin
        index := NativeUInt(entry.ItemIndex);
        bitHelper[index shr 5] := bitHelper[index shr 5] or (1 shl (Byte(index) and 31));
      end;
    end;

    for i := 0 to originalCount - 1 do
      if (TItems(fHashTable.Items)[i].HashCode >= 0)
        and (bitHelper[i shr 5] and (1 shl (Byte(i) and 31)) = 0) then
        Remove(TItems(fHashTable.Items)[i].Item);
  finally
    if heapAlloc then
      FreeMem(bitHelper);
  end;
end;

function THashSet<T>.Remove(const item: T): Boolean;
var
  temp: Pointer;
  entry: PItem;
begin
  temp := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
  if not Assigned(temp) then Exit(Boolean(Pointer(temp)));
  entry := temp;
  if Assigned(Notify) then
    Notify(Self, entry.Item, caRemoved);
  entry.Item := Default(T);
  Result := True;
end;

function THashSet<T>.ToArray: TArray<T>;
var
  target: ^T;
  source: PItem;
  i: Integer;
begin
  SetLength(Result, fHashTable.Count);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do //FI:W528
    begin
      if source.HashCode >= 0 then
      begin
        target^ := source.Item;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

procedure THashSet<T>.ExceptWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).RemoveRange(other);
end;

procedure THashSet<T>.IntersectWith(const other: IEnumerable<T>);
var
  count: NativeInt;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // Intersection of anything with empty set is empty set, so return if count is 0.
  // Same if the set intersecting with itself is the same set.
  if (fHashTable.Count = 0) or (other = IEnumerable<T>(this)) then
    Exit;

  count := other.GetNonEnumeratedCount;
  // If other is known to be empty, intersection is empty set; remove all elements, and we're done.
  if count = 0 then
  begin
    ICollection<T>(this).Clear;
    Exit;
  end;

  IntersectWithEnumerable(other);
end;

procedure THashSet<T>.UnionWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).AddRange(other);
end;

function THashSet<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  count, uniqueCount, unfoundCount: Integer;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  count := fHashTable.Count;
  // The empty set is a subset of any set, and a set is a subset of itself.
  if (count = 0) or (other = IEnumerable<T>(this)) then
    Exit(True);

  // If this has more elements then it can't be a subset.
  if Cardinal(count) > Cardinal(other.GetNonEnumeratedCount) then
    Exit(False);

  CheckUniqueAndUnfoundElements(other, False, uniqueCount, unfoundCount);
  Result := uniqueCount = count;
end;

function THashSet<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  count: Integer;
  find: TFindMethod<T>;
  enumerator: IEnumerator<T>;
  {$IFDEF MANAGED_TYPE_RVO_BROKEN}
  item: T;
  {$ENDIF}
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // A set is always a superset of itself.
  if other = IEnumerable<T>(this) then
    Exit(True);

  count := other.GetNonEnumeratedCount;
  // If other is the empty set then this is a superset.
  if count = 0 then
    Exit(True);

  TMethod(find).Data := @fHashTable;
  TMethod(find).Code := fHashTable.Find;
  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := Assigned(find(item));
    end else{$ENDIF}
    Result := Assigned(find(enumerator.Current));
    if not Result then
      Exit;
  end;

  Result := True;
end;

function THashSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  count, otherCount, uniqueCount, unfoundCount: Integer;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // A set is equal to itself.
  if other = IEnumerable<T>(this) then
    Exit(True);

  count := fHashTable.Count;
  otherCount := other.GetNonEnumeratedCount;
  // If this is empty, they are equal if other is empty as well
  if (count = 0) and (otherCount >= 0) then
    Exit(otherCount = 0);

  // Cannot be equal if other contains fewer elements than this
  if Cardinal(count) > Cardinal(otherCount) then
    Exit(False);

  CheckUniqueAndUnfoundElements(other, True, uniqueCount, unfoundCount);
  Result := (uniqueCount = count) and (unfoundCount = 0);
end;

function THashSet<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  find: TFindMethod<T>;
  enumerator: IEnumerator<T>;
  {$IFDEF MANAGED_TYPE_RVO_BROKEN}
  item: T;
  {$ENDIF}
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  if fHashTable.Count = 0 then
    Exit(False);

  // Set overlaps itself
  if other = IEnumerable<T>(this) then
    Exit(True);

  TMethod(find).Data := @fHashTable;
  TMethod(find).Code := fHashTable.Find;
  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := Assigned(find(item));
    end else{$ENDIF}
    Result := Assigned(find(enumerator.Current));
    if Result then
      Exit;
  end;

  Result := False;
end;

{$ENDREGION}


{$REGION 'THashSet<T>.TEnumerator'}

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem.Item;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if fIndex >= hashTable.ItemCount then
        Break;

      item := @TItems(hashTable.Items)[fIndex];
      Inc(fIndex);
      if item.HashCode >= 0 then
      begin
        fItem := item;
        Exit(True);
      end;
    until False;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

constructor TSortedSet<T>.Create(elementType: PTypeInfo; const comparer: IComparer<T>);
begin
  fElementType := elementType;
  fComparer := comparer;
end;

procedure TSortedSet<T>.AfterConstruction;
begin
  inherited AfterConstruction;

  fTree := TRedBlackTreeBase<T>.Create(fComparer);
end;

procedure TSortedSet<T>.BeforeDestruction;
begin
  Clear;
  fTree.Free;
  inherited BeforeDestruction;
end;

function TSortedSet<T>.Add(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.AddNode(item);
  if not Assigned(node) then Exit(Boolean(node));
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then
    Notify(Self, PNode(node).Key, caAdded);
  Result := True;
end;

procedure TSortedSet<T>.Clear;
var
  node: Pointer;
begin
  if fTree.Count = 0 then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then // optimization: if no notification needs to be send the entire tree traversal won't be done
  begin
    node := fTree.Root.LeftMost;
    fTree.ClearCount;
    Reset;
    while Assigned(node) do
    begin
      Notify(Self, PNode(node).Key, caRemoved);
      node := PBinaryTreeNode(node).Next;
    end;
  end;

  fTree.Clear;
end;

function TSortedSet<T>.Contains(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  Result := Assigned(node);
end;

function TSortedSet<T>.Extract(const item: T): T;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := PNode(node).Key;
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    DoNotify(Result, caExtracted);
  end
  else
    Result := Default(T);
end;

function TSortedSet<T>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedSet<T>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TSortedSet<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.Remove(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  if not Assigned(node) then Exit(Boolean(node));
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fTree.DeleteNode(node);
  DoNotify(item, caRemoved);
  Result := True;
end;

function TSortedSet<T>.ToArray: TArray<T>;
var
  tree: TBinaryTree;
  i: Integer;
  node: PBinaryTreeNode;
begin
  tree := fTree;
  SetLength(Result, tree.Count);
  i := 0;
  node := tree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := PNode(node).Key;
    node := node.Next;
    Inc(i);
  end;
end;

procedure TSortedSet<T>.ExceptWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).RemoveRange(other);
end;

procedure TSortedSet<T>.IntersectWith(const other: IEnumerable<T>);
var
  count: NativeInt;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // Intersection of anything with empty set is empty set, so return if count is 0.
  // Same if the set intersecting with itself is the same set.
  if (fTree.Count = 0) or (other = IEnumerable<T>(this)) then
    Exit;

  count := other.GetNonEnumeratedCount;
  // If other is known to be empty, intersection is empty set; remove all elements, and we're done.
  if count = 0 then
  begin
    ICollection<T>(this).Clear;
    Exit;
  end;

  IntersectWithEnumerable(other);
end;

procedure TSortedSet<T>.IntersectWithEnumerable(const other: IEnumerable<T>);
var
  count, capacity: NativeInt;
  enumerator: IEnumerator<T>;
  items: TArray<T>;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  count := 0;
  capacity := 0;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    if count >= capacity then
      capacity := DynArrayGrow(Pointer(items), TypeInfo(TArray<T>), capacity);
    {$IFDEF MANAGED_TYPE_RVO}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(items[count])
    else{$ENDIF}
    items[count] := enumerator.Current;
    Inc(count, Ord(not other.Contains(items[count])));
  end;
  if count > 0 then
  begin
    SetLength(items, count);
    ICollection<T>(this).RemoveRange(items);
  end;
end;

procedure TSortedSet<T>.UnionWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).AddRange(other);
end;

function TSortedSet<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF MANAGED_TYPE_RVO_BROKEN}
  item: T;
  {$ENDIF}
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // TODO: optimize
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := other.Contains(item);
    end else{$ENDIF}
    Result := other.Contains(enumerator.Current);
    if not Result then
      Exit;
  end;

  Result := True;
end;

function TSortedSet<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF MANAGED_TYPE_RVO_BROKEN}
  item: T;
  {$ENDIF}
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  // TODO: optimize
  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := IEnumerable<T>(this).Contains(item);
    end else{$ENDIF}
    Result := IEnumerable<T>(this).Contains(enumerator.Current);
    if not Result then
      Exit;
  end;

  Result := True;
end;

function TSortedSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: ISet<T>;
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  if other = IEnumerable<T>(this) then
    Exit(True);

  localSet := TSortedSet<T>.Create(fElementType, fComparer);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else{$ENDIF}
    item := enumerator.Current;
    localSet.Add(item);
    Result := IEnumerable<T>(this).Contains(item);
    if not Result then
      Exit;
  end;

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := localSet.Contains(item);
    end else{$ENDIF}
    Result := localSet.Contains(enumerator.Current);
    if not Result then
      Exit;
  end;

  Result := True;
end;

function TSortedSet<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF MANAGED_TYPE_RVO_BROKEN}
  item: T;
  {$ENDIF}
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF MANAGED_TYPE_RVO_BROKEN}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Result := IEnumerable<T>(this).Contains(item);
    end else{$ENDIF}
    Result := IEnumerable<T>(this).Contains(enumerator.Current);
    if Result then
      Exit;
  end;

  Result := False;
end;


{$ENDREGION}


{$REGION 'TSortedSet<T>.TEnumerator'}

function TSortedSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := PNode(fNode).Key;
end;

function TSortedSet<T>.TEnumerator.MoveNext: Boolean;
var
  node: Pointer;
begin
  if fVersion = fSource.fVersion then
  begin
    if fNode <> Pointer(1) then
    begin
      if Assigned(fNode) then
        node := fNode.Next
      else
        node := fSource.fTree.Root.LeftMost;
      if Assigned(node) then
      begin
        fNode := node;
        Exit(True);
      end;
      fNode := Pointer(1);
    end;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

procedure TSortedSet<T>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedSet<T>.TrimExcess;
begin
  fTree.TrimExcess;
end;

{$ENDREGION}


end.
