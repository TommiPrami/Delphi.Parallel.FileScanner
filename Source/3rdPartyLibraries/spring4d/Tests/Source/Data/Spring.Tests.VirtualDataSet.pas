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

unit Spring.Tests.VirtualDataSet;

interface

uses
  DB,
  Spring.Testing,
  Spring.Data.VirtualDataSet;

type
  TTestDataSet = class(TBaseVirtualDataSet)
  private
    fRecordCount: Integer;
    fFieldValue: Variant;
  protected
    procedure UpdateFilter; override;
    function GetRecordCount: Integer; override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
  public
    procedure SetFieldValue(const field: TField; const value: Variant);
    procedure SetRecordCount(const value: Integer);
    procedure ForceNewValueState;
  end;

  TTestVirtualDataSetBufferBounds = class(TTestCase)
  private
    procedure TestOverlongValue(wide: Boolean; const expected: string);
  published
    procedure TestWideStringFieldTruncatesOverlongValue;
    procedure TestStringFieldTruncatesOverlongValue;
    procedure TestSetFieldDataInNewValueStateWithoutEdit;
  end;

implementation

uses
  SysUtils,
  TestFramework,
  Spring.TestUtils;


{$REGION 'TTestDataSet'}

procedure TTestDataSet.UpdateFilter;
begin
end;

procedure TTestDataSet.SetFieldValue(const field: TField; const value: Variant);
var
  recBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(recBuf) then
    PRecordBufferData(recBuf).Values[field.Index] := value;
end;

function TTestDataSet.GetRecordCount: Integer;
begin
  Result := fRecordCount;
end;

procedure TTestDataSet.DoGetFieldValue(Field: TField; Index: Integer;
  var Value: Variant);
begin
  Value := fFieldValue;
end;

procedure TTestDataSet.SetRecordCount(const value: Integer);
begin
  fRecordCount := value;
end;

procedure TTestDataSet.ForceNewValueState;
begin
  SetState(dsNewValue);
end;

{$ENDREGION}


{$REGION 'TTestVirtualDataSetBufferBounds'}

procedure TTestVirtualDataSetBufferBounds.TestOverlongValue(wide: Boolean;
  const expected: string);
var
  ds: TTestDataSet;
  field: TField;
begin
  ds := TTestDataSet.Create(nil);
  try
    if wide then
    begin
      field := TWideStringField.Create(ds);
      TWideStringField(field).Size := 5;
    end
    else
    begin
      field := TStringField.Create(ds);
      TStringField(field).Size := 5;
    end;
    field.FieldName := 'Name';
    field.DataSet := ds;
    ds.FieldOptions.AutoCreateMode := acExclusive;
    ds.Active := True;
    ds.Append;
    ds.SetFieldValue(field, StringOfChar('A', 300));
    CheckEquals(expected, field.AsString);
    ds.Cancel;
  finally
    ds.Free;
  end;
end;

procedure TTestVirtualDataSetBufferBounds.TestWideStringFieldTruncatesOverlongValue;
begin
  TestOverlongValue(True, 'AAAAA');
end;

procedure TTestVirtualDataSetBufferBounds.TestStringFieldTruncatesOverlongValue;
begin
  TestOverlongValue(False, 'AAAAA');
end;

procedure TTestVirtualDataSetBufferBounds.TestSetFieldDataInNewValueStateWithoutEdit;
var
  ds: TTestDataSet;
  field: TStringField;
begin
  ds := TTestDataSet.Create(nil);
  try
    field := TStringField.Create(ds);
    field.FieldName := 'Name';
    field.Size := 10;
    field.DataSet := ds;
    ds.FieldOptions.AutoCreateMode := acExclusive;
    ds.SetRecordCount(1);
    ds.Active := True;
    ds.First;
    ds.ForceNewValueState;
    field.AsString := 'new';
  finally
    ds.Free;
  end;
  Pass;
end;

{$ENDREGION}


end.
