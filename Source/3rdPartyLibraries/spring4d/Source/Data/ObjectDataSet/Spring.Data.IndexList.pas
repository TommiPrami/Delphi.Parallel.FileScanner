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

unit Spring.Data.IndexList;

interface

uses
  Spring.Collections;

type
  TIndexList = class
  private type
    TComparison = function(const left, right: TObject): Integer of object;
  private
    fDataList: IObjectList;
    fIndexes: IList<Pointer>;
    fIsChanging: Boolean;
    function GetCount: Integer;
    function GetItem(index: Integer): TObject;
    procedure SetDataList(const value: IObjectList);
  public
    constructor Create;

    function AddItem(const item: TObject): Integer;
    procedure DeleteItem(index: Integer);
    procedure InsertItem(const item: TObject; index: Integer);

    function Contains(const item: TObject): Boolean;
    function IndexOf(const item: TObject): Integer;

    procedure Clear;
    procedure Rebuild;

    procedure InsertionSort(startIndex: Integer; const comparer: TComparison);
    procedure MergeSort(const comparer: TComparison);

    property Count: Integer read GetCount;
    property DataList: IObjectList read fDataList write SetDataList;
    property IsChanging: Boolean read fIsChanging;
    property Items[index: Integer]: TObject read GetItem; default;
  end;

implementation


{$REGION 'TIndexList'}

constructor TIndexList.Create;
begin
  inherited Create;
  fIndexes := TCollections.CreateList<Pointer>;
end;

function TIndexList.AddItem(const item: TObject): Integer;
begin
  fIsChanging := True;
  try
    fDataList.Add(item);
    Result := fIndexes.Add(item);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.Clear;
begin
  fIndexes.Clear;
end;

function TIndexList.Contains(const item: TObject): Boolean;
begin
  Result := IndexOf(item) >= 0;
end;

procedure TIndexList.DeleteItem(index: Integer);
begin
  fIsChanging := True;
  try
    fDataList.Remove(fIndexes.ExtractAt(index));
  finally
    fIsChanging := False;
  end;
end;

function TIndexList.GetCount: Integer;
begin
  Result := fIndexes.Count;
end;

function TIndexList.GetItem(index: Integer): TObject;
begin
  Result := fIndexes[index];
end;

function TIndexList.IndexOf(const item: TObject): Integer;
begin
  if Assigned(item) then
    Result := fIndexes.IndexOf(item)
  else
    Result := -1;
end;

procedure TIndexList.InsertItem(const item: TObject; index: Integer);
begin
  fIsChanging := True;
  try
    fDataList.Add(item);
    fIndexes.Insert(index, item);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.InsertionSort(startIndex: Integer;
  const comparer: TComparison);
var
  i, j: Integer;
  temp: TObject;
begin
  startIndex := startIndex - 1;
  if startIndex < 0 then
    startIndex := 0;

  for i := startIndex + 1 to Count - 1 Do
  begin
    temp := fIndexes[i];
    j := i;
    while (j > 0) and (comparer(fIndexes[j - 1], temp) > 0) do
    begin
      fIndexes[j] := fIndexes[j - 1];
      Dec(j);
    end;
    fIndexes[j] := temp;
  end;
end;

procedure TIndexList.MergeSort(const comparer: TComparison);
var
  cache: TArray<TObject>;

  procedure Merge(low, mid, high: Integer);
  var
    i, j, k: Integer;
  begin
    for i := low to high do
      cache[i] := fIndexes[i];
    i := low;
    j := mid + 1;
    k := low;
    while (i <= mid) and (j <= high) do
    begin
      if comparer(cache[i], cache[j]) <= 0 then
      begin
        fIndexes[k] := cache[i];
        Inc(i);
      end
      else
      begin
        fIndexes[k] := cache[j];
        Inc(j);
      end;
      Inc(k);
    end;

    while i <= mid do
    begin
      fIndexes[k] := cache[i];
      Inc(k);
      Inc(i);
    end;
  end;

  procedure PerformMergeSort(low, high: Integer);
  var
    mid: Integer;
  begin
    if low < high then
    begin
      mid := (high + low) div 2;
      PerformMergeSort(low, mid);
      PerformMergeSort(mid + 1, high);
      Merge(low, mid, high);
    end;
  end;

begin
  SetLength(cache, Count);
  PerformMergeSort(0, Count - 1);
end;

procedure TIndexList.Rebuild;
begin
  Clear;
  if Assigned(fDataList) then
    fIndexes.AddRange(IEnumerable<Pointer>(fDataList));
end;

procedure TIndexList.SetDataList(const value: IObjectList);
begin
  fDataList := value;
  Rebuild;
end;

{$ENDREGION}


end.
