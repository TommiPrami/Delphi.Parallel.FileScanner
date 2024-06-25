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

unit Spring.Collections.MultiSets;

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
  TAbstractMultiSet<T> = class abstract(TCollectionBase<T>)
  private type
    TEntry = TMultiSetEntry<T>;
  private
    fCount: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
    function CreateMultiSet: IMultiSet<T>; virtual; abstract;
  public
  {$REGION 'Implements IMultiSet<T>'}
    function OrderedByCount: IReadOnlyMultiSet<T>;
    function SetEquals(const other: IEnumerable<T>): Boolean;
  {$ENDREGION}
  end;

  THashMultiSetItem<T> = packed record
  public
    HashCode: Integer;
    Item: T;
    Count: Integer;
  end;

  THashMultiSet<T> = class(TAbstractMultiSet<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyMultiSet<T>,
    ICollection<T>, IMultiSet<T>)
  private type
  {$REGION 'Nested Types'}
    TEntry = TMultiSetEntry<T>;
    TItem = THashMultiSetItem<T>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: THashMultiSet<T>;
      fItemIndex: Integer;
      fRemainingCount: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fItems: THashMapInnerCollection;
    fEntries: THashMapInnerCollection;
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}
    procedure ClearWithNotify;
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create(const comparer: IEqualityComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean; overload;
    function Remove(const item: T): Boolean; overload;
    procedure Clear;
    function Extract(const item: T): T;
  {$ENDREGION}

  {$REGION 'Implements IMultiSet<T>'}
    function Add(const item: T; count: Integer): Integer; overload;
    function Remove(const item: T; count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TTreeMultiSet<T> = class(TAbstractMultiSet<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyMultiSet<T>,
    ICollection<T>, IMultiSet<T>)
  private type
  {$REGION 'Nested Types'}
    TEntry = TMultiSetEntry<T>;
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<T, Integer>.TNode
      Parent, Right, Left: Pointer;
      Key: T;
      Count: Integer;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TTreeMultiSet<T>;
      fNode: PNode;
      fRemainingCount: Integer;
      fVersion: Integer;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<T, Integer>;
    fVersion: Integer;
    fItems: TTreeMapInnerCollection;
    fEntries: TTreeMapInnerCollection;
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create(const comparer: IComparer<T>);
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean; overload;
    function Remove(const item: T): Boolean; overload;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMultiSet<T>'}
    function Add(const item: T; count: Integer): Integer; overload;
    function Remove(const item: T; count: Integer): Integer; overload;
  {$ENDREGION}
  end;

implementation

uses
  Spring.Comparers,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractMultiSet<T>'}

function TAbstractMultiSet<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAbstractMultiSet<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TAbstractMultiSet<T>.OrderedByCount: IReadOnlyMultiSet<T>;
var
  entries: TArray<TEntry>;
  items: TArray<TPair<Integer,TEntry>>;
  i: Integer;
  localSet: IMultiSet<T>;
begin
  entries := IMultiSet<T>(this).Entries.ToArray;
  SetLength(items, DynArrayLength(entries));
  for i := 0 to DynArrayHigh(entries) do
  begin
    items[i].Key := i;
    items[i].Value := entries[i];
  end;
  TArray.Sort<TPair<Integer,TMultiSetEntry<T>>>(items,
    function(const left, right: TPair<Integer,TEntry>): Integer
    begin
      if left.Value.Count > right.Value.Count then
        Result := -1
      else if left.Value.Count < right.Value.Count then
        Result := 1
      else if left.Key < right.Key then
        Result := -1
      else
        Result := 1;
    end);
  localSet := THashMultiSet<T>.Create(nil);
  for i := 0 to DynArrayHigh(items) do
    localSet.Add(items[i].Value.Item, items[i].Value.Count);
  Result := localSet as IReadOnlyMultiSet<T>;
end;

function TAbstractMultiSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: IMultiSet<T>;
  entry: TEntry;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  localSet := CreateMultiSet;
  localSet.AddRange(other);

  if fCount <> localSet.Count then
    Exit(False);
  for entry in localSet.Entries do
    if IMultiSet<T>(this)[entry.Item] <> entry.Count then
      Exit(False);
  Result := True;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>'}

constructor THashMultiSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  fHashTable.Comparer := comparer;
end;

procedure THashMultiSet<T>.AfterConstruction;
var
  elementType: PTypeInfo;
begin
  inherited AfterConstruction;

  elementType := GetElementType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<T>.Equals, @TComparerThunks<T>.GetHashCode, elementType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<T>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<T>.FindWithComparer;

  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass:
    begin
      fItems := THashMapInnerCollection.Create_Object(
        Self, @fHashTable, nil, elementType, 0);
      fEntries := THashMapInnerCollection.Create(THashMapInnerCollection<TPair<TObject,Integer>>,
        Self, @fHashTable, nil, TypeInfo(TEntry), 0);
    end;
    tkInterface:
    begin
      fItems := THashMapInnerCollection.Create_Interface(
        Self, @fHashTable, nil, elementType, 0);
      fEntries := THashMapInnerCollection.Create(THashMapInnerCollection<TPair<IInterface,Integer>>,
        Self, @fHashTable, nil, TypeInfo(TEntry), 0);
    end;
  else{$ELSE}begin{$ENDIF}
    fItems := THashMapInnerCollection.Create(THashMapInnerCollection<T>,
      Self, @fHashTable, nil, elementType, 0);
    fEntries := THashMapInnerCollection.Create(THashMapInnerCollection<TPair<T,Integer>>,
      Self, @fHashTable, nil, TypeInfo(TEntry), 0);
  end;
end;

procedure THashMultiSet<T>.BeforeDestruction;
begin
  Clear;
  fEntries.Free;
  fItems.Free;
  inherited BeforeDestruction;
end;

function THashMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(IEqualityComparer<T>(fHashTable.Comparer));
end;

function THashMultiSet<T>.Add(const item: T): Boolean;
begin
  Add(item, 1);
  Result := True;
end;

function THashMultiSet<T>.Add(const item: T; count: Integer): Integer;
var
  entry: PItem;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  entry := IHashTable<T>(@fHashTable).Find(item, OverwriteExisting or InsertNonExisting);
  entry.Item := item;
  if entry.HashCode < 0 then
  begin
    entry.HashCode := entry.HashCode and not RemovedFlag;
    Result := entry.Count;
    Inc(entry.Count, count);
  end
  else
  begin
    Result := 0;
    entry.Count := count;
  end;
  Inc(fCount, count);

  if Assigned(Notify) then
    for i := 1 to count do //FI:W528
      Notify(Self, item, caAdded);
end;

procedure THashMultiSet<T>.Clear;
begin
  if not Assigned(Notify) then
    fHashTable.Clear
  else
    ClearWithNotify;
end;

procedure THashMultiSet<T>.ClearWithNotify;
var
  oldItemCount, i, n: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);

  fHashTable.Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
      for n := 1 to oldItems[i].Count do //FI:W528
        Notify(Self, oldItems[i].Item, caRemoved);
end;

function THashMultiSet<T>.Contains(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  Result := Assigned(entry);
end;

function THashMultiSet<T>.Extract(const item: T): T;
var
  previousCount: Integer;
begin
  previousCount := Remove(item, 1); // TODO: possibly change if/when implementing ownership
  if previousCount > 0 then
    Result := item
  else
    Result := Default(T);
end;

function THashMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := IReadOnlyCollection<TEntry>(fEntries._this);
end;

function THashMultiSet<T>.GetItems: IReadOnlyCollection<T>;
begin
  Result := IReadOnlyCollection<T>(fItems._this);
end;

function THashMultiSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function THashMultiSet<T>.GetItemCount(const item: T): Integer;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  if not Assigned(entry) then Exit(Integer(Pointer(entry)));
  Result := entry.Count;
end;

function THashMultiSet<T>.Remove(const item: T): Boolean;
var
  previousCount: Integer;
begin
  previousCount := Remove(item, 1);
  Result := previousCount > 0;
end;

function THashMultiSet<T>.Remove(const item: T; count: Integer): Integer;
var
  entry: THashTableEntry;
  tableItem: PItem;
  existingCount, i: Integer;
begin
  if count >= 0 then
  begin
    entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(item);
    Result := Ord(fHashTable.FindEntry(item, entry));
    if Result = 0 then Exit;
    tableItem := @TItems(fHashTable.Items)[entry.ItemIndex];
    existingCount := tableItem.Count;
    if existingCount <= count then
    begin
      fHashTable.DeleteEntry(entry);
      count := existingCount;
    end
    else
      Dec(tableItem.Count, count);
    Dec(fCount, count);

    if Assigned(Notify) then
      for i := 1 to count do //FI:W528
        Notify(Self, item, caRemoved);
    Result := existingCount;
  end
  else
    Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

procedure THashMultiSet<T>.SetItemCount(const item: T; count: Integer);
var
  entry: PItem;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  if count = 0 then
  begin
    entry := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
    if Assigned(entry) then
    begin
      Dec(fCount, entry.Count);
      if Assigned(Notify) then
        for i := 1 to entry.Count do //FI:W528
          Notify(Self, item, caRemoved);
    end;
  end
  else
  begin
    entry := IHashTable<T>(@fHashTable).Find(item, InsertNonExisting);
    entry.Item := item;
//    entry.Count := 0;
    Inc(fCount, count - entry.Count);
    i := entry.Count;
    entry.Count := count;
    if Assigned(Notify) then
      while i > count do
      begin
        Notify(Self, item, caRemoved);
        Dec(i);
      end;
      while i < count do
      begin
        Notify(Self, item, caAdded);
        Inc(i);
      end;
  end;
end;

function THashMultiSet<T>.ToArray: TArray<T>;
var
  target: ^T;
  source: PItem;
  i, count: Integer;
begin
  SetLength(Result, fCount);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do
    begin
      if source.HashCode >= 0 then
        for count := 1 to source.Count do //FI:W528
        begin
          target^ := source.Item;
          Inc(target);
        end;
      Inc(source);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>.TEnumerator'}

function THashMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem.Item;
end;

function THashMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    if fRemainingCount > 0 then
    begin
      Dec(fRemainingCount);
      Exit(True);
    end;

    while fItemIndex < hashTable.ItemCount do
    begin
      item := @TItems(hashTable.Items)[fItemIndex];
      Inc(fItemIndex);
      if item.HashCode >= 0 then
      begin
        fItem := item;
        fRemainingCount := item.Count - 1;
        Exit(True);
      end;
    end;

    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>'}

constructor TTreeMultiSet<T>.Create(const comparer: IComparer<T>);
begin
  fTree := TRedBlackTreeBase<T, Integer>.Create(comparer);
  fItems := TTreeMapInnerCollection.Create(TTreeMapInnerCollection<T>,
    Self, fTree, @fVersion, nil, GetElementType, 0);
  fEntries := TTreeMapInnerCollection.Create(TTreeMapInnerCollection<TPair<T,Integer>>,
    Self, fTree, @fVersion, nil, TypeInfo(TEntry), 0);
end;

procedure TTreeMultiSet<T>.BeforeDestruction;
begin
  Clear;
  fEntries.Free;
  fItems.Free;
  fTree.Free;
  inherited BeforeDestruction;
end;

function TTreeMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(IComparer<T>(fTree.Comparer));
end;

function TTreeMultiSet<T>.Add(const item: T): Boolean;
begin
  Add(item, 1);
  Result := True;
end;

function TTreeMultiSet<T>.Add(const item: T; count: Integer): Integer;
var
  node: Pointer;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := PNode(node).Count;
    PNode(node).Count := Result + count;
  end
  else
  begin
    node := fTree.AddNode(item);
    PNode(node).Count := count;
    Result := 0;
  end;
  Inc(fCount, count);

  if Assigned(Notify) then
    for i := 1 to count do //FI:W528
      Notify(Self, item, caAdded);
end;

procedure TTreeMultiSet<T>.Clear;
var
  node: Pointer;
  count: Integer;
begin
  if fCount > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    if Assigned(Notify) then
    begin
      node := fTree.Root.LeftMost;
      while Assigned(node) do
      begin
        for count := PNode(node).Count downto 1 do //FI:W528
          Notify(Self, PNode(node).Key, caRemoved);
        node := PBinaryTreeNode(node).Next;
      end;
    end;

    fTree.Clear;
    fCount := 0;
  end;
end;

function TTreeMultiSet<T>.Contains(const value: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(value);
  Result := Assigned(node);
end;

function TTreeMultiSet<T>.Extract(const item: T): T;
var
  previousCount: Integer;
begin
  previousCount := Remove(item, 1); // TODO: possibly change if/when implementing ownership
  if previousCount > 0 then
    Result := item
  else
    Result := Default(T);
end;

function TTreeMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := IReadOnlyCollection<TEntry>(fEntries._this);
end;

function TTreeMultiSet<T>.GetItems: IReadOnlyCollection<T>;
begin
  Result := IReadOnlyCollection<T>(fItems._this);
end;

function TTreeMultiSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TTreeMultiSet<T>.GetItemCount(const item: T): Integer;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  if not Assigned(node) then Exit(Integer(node)); //FI:W541
  Result := PNode(node).Count;
end;

function TTreeMultiSet<T>.Remove(const item: T): Boolean;
var
  previousCount: Integer;
begin
  previousCount := Remove(item, 1);
  Result := previousCount > 0;
end;

function TTreeMultiSet<T>.Remove(const item: T; count: Integer): Integer;
var
  temp: Pointer;
  node: PNode;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  temp := fTree.FindNode(item);
  if not Assigned(temp) then Exit(Integer(temp)); //FI:W541
  node := temp;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  Result := node.Count;
  if Result <= count then
  begin
    fTree.DeleteNode(Pointer(node));
    count := Result;
  end
  else
    node.Count := Result - count;
  Dec(fCount, count);
  if Assigned(Notify) then
    for i := 1 to count do //FI:W528
      Notify(Self, item, caRemoved);
end;

procedure TTreeMultiSet<T>.SetItemCount(const item: T; count: Integer);
var
  node: Pointer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if count = 0 then
  begin
    node := fTree.FindNode(item);
    if Assigned(node) then
      fTree.DeleteNode(node);
  end
  else
  begin
    node := fTree.AddNode(item, True);
    node := Pointer(IntPtr(node) and not 1);
    PNode(node).Count := count;
  end;
end;

function TTreeMultiSet<T>.ToArray: TArray<T>;
var
  node: Pointer;
  index, count: Integer;
begin
  SetLength(Result, fCount);
  index := 0;
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    for count := 1 to PNode(node).Count do //FI:W528
    begin
      Result[index] := PNode(node).Key;
      Inc(index);
    end;
    node := PBinaryTreeNode(node).Next;
  end;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TEnumerator'}

function TTreeMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fNode.Key;
end;

function TTreeMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  node: Pointer;
begin
  if fVersion = fSource.fVersion then
  begin
    if fRemainingCount = 0 then
    begin
      if not Assigned(fNode) then
        node := fSource.fTree.Root.LeftMost
      else
        node := PBinaryTreeNode(fNode).Next;

      Result := Assigned(node);
      if Result then
      begin
        fNode := node;
        fRemainingCount := PNode(node).Count - 1;
      end;
    end
    else
    begin
      Dec(fRemainingCount);
      Result := True;
    end;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


end.
