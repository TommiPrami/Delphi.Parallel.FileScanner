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

unit Spring.Sorting;

interface

type
  TSort_Int8 = record
  private type
    TDataType = Int8;
    PDataType = ^Int8;
  const
    Shift = 0;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_UInt8 = record
  private type
    TDataType = UInt8;
    PDataType = ^UInt8;
  const
    Shift = 0;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Int16 = record
  private type
    TDataType = Int16;
    PDataType = ^Int16;
  const
    Shift = 1;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_UInt16 = record
  private type
    TDataType = UInt16;
    PDataType = ^UInt16;
  const
    Shift = 1;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Int32 = record
  private type
    TDataType = Int32;
    PDataType = ^Int32;
  const
    Shift = 2;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_UInt32 = record
  private type
    TDataType = UInt32;
    PDataType = ^UInt32;
  const
    Shift = 2;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Int64 = record
  private type
    TDataType = Int64;
    PDataType = ^Int64;
  const
    Shift = 3;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_UInt64 = record
  private type
    TDataType = UInt64;
    PDataType = ^UInt64;
  const
    Shift = 3;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Single = record
  private type
    TDataType = Single;
    PDataType = ^Single;
  const
    Shift = 2;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Double = record
  private type
    TDataType = Double;
    PDataType = ^Double;
  const
    Shift = 3;
  public
    class procedure HeapSort(lo, hi: PDataType); static;
    class procedure InsertionSort(lo, hi: PDataType); static;
    class procedure UnguardedInsertionSort(lo, hi: PDataType); static;
    class function PartialInsertionSort(lo, hi: PDataType): Boolean; static;
    class function PartitionLeft(lo, hi: PDataType): PDataType; static;
    class function PartitionRight(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class function PartitionRightBranchless(lo, hi: PDataType; out pivotPos: PDataType): Boolean; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PDataType; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True; branchless: Boolean = False); static;
  end;

  TSort_Ref = record
  public type
    TCompareMethod = function(const left, right: Pointer): Integer of object;
    TLessThanFunc = function(const left, right: Pointer): Boolean;
  private
    class procedure HeapSort(lo: PByte; hi: NativeUInt;
      const compare: TCompareMethod; size: NativeUInt); overload; static;
    class procedure InsertionSort(lo, hi: PByte; size: NativeInt;
      const compare: TCompareMethod); overload; static;
    class procedure UnguardedInsertionSort(lo, hi: PByte; size: NativeInt;
      const compare: TCompareMethod); overload; static;
    class function PartialInsertionSort(lo, hi: PByte; size: NativeUInt;
      const compare: TCompareMethod): Boolean; overload; static;
    class function PartitionLeft(lo, hi: PByte; size: NativeInt;
      const compare: TCompareMethod): PByte; overload; static;
    class function PartitionRight(lo, hi: PByte; size: NativeInt;
      const compare: TCompareMethod; out pivotPos: PByte): Boolean; overload; static;
    class procedure HeapSort(lo: PByte; hi: NativeUInt;
      const compare: TLessThanFunc; size: NativeUInt); overload; static;
    class procedure InsertionSort(lo, hi: PByte; size: NativeInt;
      const compare: TLessThanFunc); overload; static;
    class procedure UnguardedInsertionSort(lo, hi: PByte; size: NativeInt;
      const compare: TLessThanFunc); overload; static;
    class function PartialInsertionSort(lo, hi: PByte; size: NativeUInt;
      const compare: TLessThanFunc): Boolean; overload; static;
    class function PartitionLeft(lo, hi: PByte; size: NativeInt;
      const compare: TLessThanFunc): PByte; overload; static;
    class function PartitionRight(lo, hi: PByte; size: NativeInt;
      const compare: TLessThanFunc; out pivotPos: PByte): Boolean; overload; static;
  public
    class procedure PatternDefeatingQuickSort(lo, hi: PByte; size: NativeUInt;
      const compare: TCompareMethod; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True); overload; static;
    class procedure PatternDefeatingQuickSort(lo, hi: PByte; size: NativeUInt;
      const compare: TLessThanFunc; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True); overload; static;
  end;

  TLessThanFunc<T> = function(const left, right: T): Boolean;
  TCompareMethod<T> = function(const left, right: T): Integer of object;
  TSort = record
  public type
    Pointer<T> = record
      private type TSlice = array[0..1] of T;
      private type TIdx = array[0..0] of T;
      private type TRec = packed record Value: T; end;
      {$POINTERMATH ON}
      public type Raw = ^T;
      public type Idx = ^TIdx;
      public type Rec = ^TRec;
      {$POINTERMATH OFF}
      public type Slice = ^TSlice;
    end;
  private{$IFNDEF DELPHIXE4_UP}{$HINTS OFF}{$ENDIF}
    class procedure PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi: Pointer<T>.Idx;
      const compare: TCompareMethod<T>; threads, depthLimit: Integer; leftMost: Boolean); overload; static;
    class procedure PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi: Pointer<T>.Idx;
      const compare: TLessThanFunc<T>; threads, depthLimit: Integer; leftMost: Boolean); overload; static;
  public{$IFNDEF DELPHIXE4_UP}{$HINTS ON}{$ENDIF}
    class procedure SiftDown<T>(lo: Pointer<T>.Idx; hi, i: NativeUInt; const compare: TCompareMethod<T>);  overload; static;
    class procedure HeapSort<T>(lo, hi: Pointer<T>.Idx; const compare: TCompareMethod<T>); overload; static;
    class procedure InsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TCompareMethod<T>); overload; static;
    class procedure UnguardedInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TCompareMethod<T>); overload; static;
    class function PartialInsertionSort<T>(lo, hi: Pointer<T>.Idx; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): Boolean; overload; static;
    class function PartitionLeft<T>(lo, hi: Pointer<T>.Idx; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): Pointer<T>.Idx; overload; static;
    class function PartitionRight<T>(lo, hi: Pointer<T>.Idx; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>; out pivotPos: Pointer<T>.Idx): Boolean; overload; static;
    class procedure PatternDefeatingQuickSort<T>(lo, hi: Pointer<T>.Idx;
      const compare: TCompareMethod<T>; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True); overload; static;

    class procedure SiftDown<T>(lo: Pointer<T>.Idx; hi, i: NativeUInt; const compare: TLessThanFunc<T>);  overload; static;
    class procedure HeapSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>); overload; static;
    class procedure InsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>); overload; static;
    class procedure UnguardedInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>); overload; static;
    class function PartialInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>): Boolean; overload; static;
    class function PartitionLeft<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>): Pointer<T>.Idx; overload; static;
    class function PartitionRight<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>; out pivotPos: Pointer<T>.Idx): Boolean; overload; static;
    class procedure PatternDefeatingQuickSort<T>(lo, hi: Pointer<T>.Idx;
      const compare: TLessThanFunc<T>; threads: Integer = 1;
      depthLimit: Integer = -1; leftMost: Boolean = True); overload; static;
  end;

implementation

uses
  Spring,
  Spring.Threading;

const
  InsertionSortThreshold = 24;
  PartialInsertionSortLimit = 8;
  NintherThreshold = 128;
  BlockSize = 64;
  CacheLineSize = 64;
  SequentialThreshold = 2000;

function AlignCacheLine(p: Pointer): Pointer; inline;
begin
  Result := Pointer((UIntPtr(p) + CacheLineSize - 1) and -CacheLineSize);
end;

{$POINTERMATH ON}
{$R-,O+,Q-,W-}


{$REGION 'TSort_Int8'}

class procedure TSort_Int8.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Int8.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Int8.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Int8.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Int8.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Int8.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Int8.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Int8.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_UInt8'}

class procedure TSort_UInt8.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_UInt8.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_UInt8.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_UInt8.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_UInt8.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_UInt8.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_UInt8.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_UInt8.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Int16'}

class procedure TSort_Int16.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Int16.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Int16.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Int16.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Int16.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Int16.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Int16.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Int16.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_UInt16'}

class procedure TSort_UInt16.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_UInt16.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_UInt16.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_UInt16.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_UInt16.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_UInt16.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_UInt16.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_UInt16.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Int32'}

class procedure TSort_Int32.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Int32.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Int32.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Int32.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Int32.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Int32.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Int32.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Int32.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_UInt32'}

class procedure TSort_UInt32.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_UInt32.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_UInt32.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_UInt32.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_UInt32.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_UInt32.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_UInt32.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_UInt32.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Int64'}

class procedure TSort_Int64.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Int64.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Int64.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Int64.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Int64.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Int64.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Int64.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Int64.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_UInt64'}

class procedure TSort_UInt64.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_UInt64.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_UInt64.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_UInt64.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_UInt64.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_UInt64.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_UInt64.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_UInt64.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Single'}

class procedure TSort_Single.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Single.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Single.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Single.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Single.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Single.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Single.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Single.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Double'}

class procedure TSort_Double.HeapSort;
{$I Spring.Sorting.HeapSort.inc}

class procedure TSort_Double.InsertionSort;
{$I Spring.Sorting.InsertionSort.inc}

class procedure TSort_Double.UnguardedInsertionSort;
{$I Spring.Sorting.UnguardedInsertionSort.inc}

class function TSort_Double.PartialInsertionSort;
{$I Spring.Sorting.PartialInsertionSort.inc}

class function TSort_Double.PartitionLeft;
{$I Spring.Sorting.PartitionLeft.inc}

class function TSort_Double.PartitionRight;
{$I Spring.Sorting.PartitionRight.inc}

class function TSort_Double.PartitionRightBranchless;
{$I Spring.Sorting.PartitionRightBranchless.inc}

class procedure TSort_Double.PatternDefeatingQuickSort;
{$I Spring.Sorting.PatternDefeatingQuickSort.inc}

{$ENDREGION}


{$REGION 'TSort_Ref'}

class procedure TSort_Ref.HeapSort(lo: PByte; hi: NativeUInt;
  const compare: TCompareMethod; size: NativeUInt);

  procedure SiftDown(lo: PByte; hi: NativeUInt;
    const compare: TCompareMethod; i, size: NativeUInt);
  var
    child: NativeUInt;
  begin
    repeat
      child := i * 2 + 1;
      if child > hi then
        Break;

      if child < hi then
        Inc(child, compare(@lo[size * child], @lo[size * child + size]) shr 31);
      if compare(@lo[i * size], @lo[child * size]) >= 0 then
        Break;

      BinarySwap(@lo[i * size], @lo[child * size], size);
      i := child;
    until False;
  end;

var
  i: NativeInt;
begin
  for i := (hi + 1) shr 1 - 1 downto 0 do
    SiftDown(lo, hi, compare, i, size);
  for i := hi downto 1 do
  begin
    BinarySwap(@lo[NativeUInt(i) * size], lo, size);
    SiftDown(lo, i - 1 , compare, 0, size);
  end;
end;

class procedure TSort_Ref.InsertionSort(lo, hi: PByte; size: NativeInt;
  const compare: TCompareMethod);
var
  cur, sift: PByte;
begin
  cur := lo + size;
  if cur < hi then
  repeat
    sift := cur;

    if compare(sift, sift - size) < 0 then
      repeat
        BinarySwap(sift, sift - size, size);
        Dec(sift, size);
      until (sift = lo) or (compare(sift, sift - size) >= 0);

    Inc(cur, size);
  until cur = hi;
end;

class procedure TSort_Ref.UnguardedInsertionSort(lo, hi: PByte; size: NativeInt;
  const compare: TCompareMethod);
var
  cur, sift: PByte;
begin
  cur := lo + size;
  if cur < hi then
  repeat
    sift := cur;

    if compare(sift, sift - size) < 0 then
      repeat
        BinarySwap(sift, sift - size, size);
        Dec(sift, size);
      until compare(sift, sift - size) >= 0;

    Inc(cur, size);
  until cur = hi;
end;

class function TSort_Ref.PartialInsertionSort(lo, hi: PByte; size: NativeUInt;
  const compare: TCompareMethod): Boolean;
var
  limit: NativeInt;
  cur, sift: PByte;
begin
  cur := lo + size;
  limit := 0;
  if cur < hi then
  repeat
    sift := cur;

    if compare(sift, sift - size) >= 0 then
    begin
      Inc(cur, size);
      if cur = hi then Break;
    end
    else
    begin
      repeat
        BinarySwap(sift, sift - size, size);
        Dec(sift, size);
      until (sift = lo) or (compare(sift, sift - size) >= 0);

      Inc(limit, (NativeUInt(cur) - NativeUInt(sift)) div size);
      if limit > PartialInsertionSortLimit then Exit(False);
      Inc(cur, size);
      if cur = hi then Break;
    end;
  until False;
  Result := True;
end;

class function TSort_Ref.PartitionLeft(lo, hi: PByte; size: NativeInt;
  const compare: TCompareMethod): PByte;
var
  first, last: PByte;
begin
  first := lo;
  last := hi;

  repeat
    Dec(last, size);
  until compare(lo, last) >= 0;

  if last + size = hi then
    repeat
      if first >= last then Break;
      Inc(first, size);
    until compare(lo, first) < 0
  else
    repeat
      Inc(first, size);
    until compare(lo, first) < 0;

  while first < last do
  begin
    BinarySwap(first, last, size);
    repeat
      Dec(last, size);
    until compare(lo, last) >= 0;
    repeat
      Inc(first, size)
    until compare(lo, first) < 0;
  end;

  BinarySwap(lo, last, size);
  Result := last;
end;

class function TSort_Ref.PartitionRight(lo, hi: PByte; size: NativeInt;
  const compare: TCompareMethod; out pivotPos: PByte): Boolean;
var
  first, last: PByte;
begin
  first := lo;
  last := hi;

  repeat
    Inc(first, size);
  until compare(first, lo) >= 0;

  if first - size = lo then
    repeat
      if first >= last then Break;
      Dec(last, size);
    until compare(last, lo) < 0
  else
    repeat
      Dec(last, size);
    until compare(last, lo) < 0;

  if first >= last then
    Result := True
  else
  begin
    repeat
      BinarySwap(first, last, size);
      repeat
        Inc(first, size);
      until compare(first, lo) >= 0;
      repeat
        Dec(last, size);
      until compare(last, lo) < 0;
    until first >= last;
    Result := False;
  end;

  Dec(first, size);
  BinarySwap(lo, first, size);
  pivotPos := first;
end;

class procedure TSort_Ref.PatternDefeatingQuickSort(lo, hi: PByte; size: NativeUInt;
  const compare: TCompareMethod; threads, depthLimit: Integer; leftMost: Boolean);

  procedure PatternDefeatingQuickSort_Async(lo, pivotPos, hi: PByte; size: NativeUInt;
    const compare: TCompareMethod; threads, depthLimit: Integer; leftMost: Boolean);
  var
    task: IInterface;
  begin
    task := ParallelAPI.RunTask(
      procedure
      begin
        PatternDefeatingQuickSort(lo, pivotPos, size, compare, threads, depthLimit, leftMost);
      end);
    PatternDefeatingQuickSort(pivotPos + size, hi, size, compare, threads, depthLimit, False);
    ParallelAPI.WaitFor(task);
  end;

var
  partitionSize, leftSize, rightSize, i: NativeUInt;
  alreadyPartitioned: Boolean;
  left, right, mid, pivotPos: PByte;
begin
  if depthLimit < 0 then
  begin
    depthLimit := Log2((NativeUInt(hi) - NativeUInt(lo)) div size);
    if threads = 0 then
      threads := CPUCount;
  end;

  repeat
    partitionSize := (NativeUInt(hi) - NativeUInt(lo)) div size;
    if partitionSize < InsertionSortThreshold then
    begin
      if not leftMost then
        UnguardedInsertionSort(lo, hi, size, compare)
      else
        InsertionSort(lo, hi, size, compare);
      Exit;
    end
    else
    begin
      left := lo;
      mid := lo + ((partitionSize shr 1) * size);
      right := hi - size;

      if compare(left, mid) < 0 then
        BinarySwap(left, mid, size);
      if compare(right, left) < 0 then
      begin
        BinarySwap(right, left, size);
        if compare(left, mid) < 0 then
          BinarySwap(left, mid, size);
      end;

      if not leftMost and (compare(lo - size, lo) >= 0) then
      begin
        lo := PartitionLeft(lo, hi, size, compare) + size;
        Continue;
      end;

      // Partition and get results.
      alreadyPartitioned := PartitionRight(lo, hi, size, compare, pivotPos);

      // Check for a highly unbalanced partition.
      leftSize := (NativeUInt(pivotPos) - NativeUInt(lo)) div size;
      rightSize := partitionSize - leftSize;

      // If we got a highly unbalanced partition we shuffle elements to break many patterns.
      if (leftSize < partitionSize shr 3) or (rightSize < partitionSize shr 3) then
      begin
        // If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
        Dec(depthLimit);
        if depthLimit = 0 then
        begin
          HeapSort(lo, partitionSize - 1, compare, size);
          Exit;
        end;

        if leftSize >= InsertionSortThreshold then
        begin
          i := leftSize shr 2;
          BinarySwap(lo, lo + (i * size), size);
          BinarySwap(pivotpos - size, pivotPos - (i * size), size);
        end;

        if rightSize >= InsertionSortThreshold then
        begin
          i := rightSize shr 2;
          BinarySwap(pivotPos + size, pivotPos + (i + 1) * size, size);
          BinarySwap(hi - size, hi - i * size, size);
        end;
      end
      else
      begin
        // If we were decently balanced and we tried to sort an already partitioned
        // sequence try to use insertion sort.
        if alreadyPartitioned
          and PartialInsertionSort(lo, pivotPos, size, compare)
          and PartialInsertionSort(pivotPos + size, hi, size, compare) then Exit;
      end;

      if (threads > 1) and ((leftSize > SequentialThreshold) or (rightSize > SequentialThreshold)) then
      begin
        PatternDefeatingQuickSort_Async(lo, pivotPos, hi, size, compare, threads shr 1, depthLimit, leftMost);
        Exit;
      end;

      PatternDefeatingQuickSort(lo, pivotPos, size, compare, 0, depthLimit, leftMost);
      lo := pivotPos + size;
      leftMost := False;
    end;
  until False;
end;

class procedure TSort_Ref.HeapSort(lo: PByte; hi: NativeUInt;
  const compare: TLessThanFunc; size: NativeUInt);

  procedure SiftDown(lo: PByte; hi: NativeUInt; const compare: TLessThanFunc;
    i, size: NativeUInt); overload;
  var
    child: NativeUInt;
  begin
    repeat
      child := i * 2 + 1;
      if child > hi then
        Break;

      if child < hi then
        Inc(child, Ord(compare(@lo[size * child], @lo[size * child + size])));
      if not compare(@lo[i * size], @lo[child * size]) then
        Break;

      BinarySwap(@lo[i * size], @lo[child * size], size);
      i := child;
    until False;
  end;

var
  i: NativeInt;
begin
  for i := (hi + 1) shr 1 - 1 downto 0 do
    SiftDown(lo, hi, compare, i, size);
  for i := hi downto 1 do
  begin
    BinarySwap(@lo[NativeUInt(i) * size], lo, size);
    SiftDown(lo, i - 1 , compare, 0, size);
  end;
end;

class procedure TSort_Ref.InsertionSort(lo, hi: PByte; size: NativeInt;
  const compare: TLessThanFunc);
var
  cur, sift: PByte;
begin
  cur := lo;
  repeat
    sift := cur;
    Inc(cur, size);
    if cur >= hi then Break;

    if compare(cur, sift) then
      repeat
        BinarySwap(@sift[size], sift, size);
        Dec(sift, size);
      until (sift < lo) or not compare(@sift[size], sift);
  until False;
end;

class procedure TSort_Ref.UnguardedInsertionSort(lo, hi: PByte; size: NativeInt;
  const compare: TLessThanFunc);
var
  cur, sift: PByte;
begin
  cur := lo;
  repeat
    sift := cur;
    Inc(cur, size);
    if cur >= hi then Break;

    if compare(cur, sift) then
      repeat
        BinarySwap(@sift[size], sift, size);
        Dec(sift, size);
      until not compare(@sift[size], sift);
  until False;
end;

class function TSort_Ref.PartialInsertionSort(lo, hi: PByte; size: NativeUInt;
  const compare: TLessThanFunc): Boolean;
var
  limit: NativeInt;
  cur, sift: PByte;
begin
  limit := PartialInsertionSortLimit * size;
  cur := lo;
  repeat
    sift := cur;
    Inc(cur, size);
    if cur >= hi then Break;

    if compare(cur, sift) then
    begin
      sift := cur;
      repeat
        Dec(sift, size);
        BinarySwap(@sift[size], sift, size);
      until (sift = lo) or not compare(sift, sift - size);

      Dec(limit, NativeUInt(cur) - NativeUInt(sift));
      if limit <= 0 then Exit(False);
    end;
  until False;
  Result := True;
end;

class function TSort_Ref.PartitionLeft(lo, hi: PByte; size: NativeInt;
  const compare: TLessThanFunc): PByte;
var
  first, last: PByte;
begin
  first := lo;
  last := hi;

  repeat
    Dec(last, size);
  until not compare(lo, last);

  if last + size = hi then
    repeat
      if first >= last then Break;
      Inc(first, size);
    until compare(lo, first)
  else
    repeat
      Inc(first, size);
    until compare(lo, first);

  while first < last do
  begin
    BinarySwap(first, last, size);
    repeat
      Dec(last, size);
    until not compare(lo, last);
    repeat
      Inc(first, size)
    until compare(lo, first);
  end;

  BinarySwap(last, lo, size);
  Result := last;
end;

class function TSort_Ref.PartitionRight(lo, hi: PByte; size: NativeInt;
  const compare: TLessThanFunc; out pivotPos: PByte): Boolean;
var
  first, last: PByte;
begin
  first := lo;
  last := hi;

  repeat
    Inc(first, size);
  until not compare(first, lo);

  if first - size = lo then
    repeat
      if first >= last then Break;
      Dec(last, size);
    until compare(last, lo)
  else
    repeat
      Dec(last, size);
    until compare(last, lo);

  if first >= last then
    Result := True
  else
  begin
    repeat
      BinarySwap(first, last, size);
      repeat
        Inc(first, size);
      until not compare(first, lo);
      repeat
        Dec(last, size);
      until compare(last, lo);
    until first >= last;
    Result := False;
  end;

  Dec(first, size);
  BinarySwap(lo, first, size);
  pivotPos := first;
end;

class procedure TSort_Ref.PatternDefeatingQuickSort(lo, hi: PByte; size: NativeUInt;
  const compare: TLessThanFunc; threads, depthLimit: Integer; leftMost: Boolean);

  procedure PatternDefeatingQuickSort_Async(lo, pivotPos, hi: PByte; size: NativeUInt;
    const compare: TLessThanFunc; threads, depthLimit: Integer; leftMost: Boolean);
  var
    task: IInterface;
  begin
    task := ParallelAPI.RunTask(
      procedure
      begin
        PatternDefeatingQuickSort(lo, pivotPos, size, compare, threads, depthLimit, leftMost);
      end);
    PatternDefeatingQuickSort(pivotPos + size, hi, size, compare, threads, depthLimit, False);
    ParallelAPI.WaitFor(task);
  end;

var
  partitionSize, leftSize, rightSize, i: NativeUInt;
  alreadyPartitioned: Boolean;
  left, right, mid, pivotPos: PByte;
begin
  if depthLimit < 0 then
  begin
    depthLimit := Log2((NativeUInt(hi) - NativeUInt(lo)) div size);
    if threads = 0 then
      threads := CPUCount;
  end;

  repeat
    partitionSize := (NativeUInt(hi) - NativeUInt(lo)) div size;
    if partitionSize < InsertionSortThreshold then
    begin
      if not leftMost then
        UnguardedInsertionSort(lo, hi, size, compare)
      else
        InsertionSort(lo, hi, size, compare);
      Exit;
    end
    else
    begin
      left := lo;
      mid := lo + ((partitionSize shr 1) * size);
      right := hi - size;

      if compare(left, mid) then
        BinarySwap(left, mid, size);
      if compare(right, left) then
      begin
        BinarySwap(right, left, size);
        if compare(left, mid) then
          BinarySwap(left, mid, size);
      end;

      if not leftMost and not compare(lo - size, lo) then
      begin
        lo := PartitionLeft(lo, hi, size, compare) + size;
        Continue;
      end;

      // Partition and get results.
      alreadyPartitioned := PartitionRight(lo, hi, size, compare, pivotPos);

      // Check for a highly unbalanced partition.
      leftSize := (NativeUInt(pivotPos) - NativeUInt(lo)) div size;
      rightSize := partitionSize - leftSize;

      // If we got a highly unbalanced partition we shuffle elements to break many patterns.
      if (leftSize < partitionSize shr 3) or (rightSize < partitionSize shr 3) then
      begin
        // If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
        Dec(depthLimit);
        if depthLimit = 0 then
        begin
          HeapSort(lo, partitionSize - 1, compare, size);
          Exit;
        end;

        if leftSize >= InsertionSortThreshold then
        begin
          i := leftSize shr 2;
          BinarySwap(lo, lo + (i * size), size);
          BinarySwap(pivotpos - size, pivotPos - (i * size), size);
        end;

        if rightSize >= InsertionSortThreshold then
        begin
          i := rightSize shr 2;
          BinarySwap(pivotPos + size, pivotPos + (i + 1) * size, size);
          BinarySwap(hi - size, hi - i * size, size);
        end;
      end
      else
      begin
        // If we were decently balanced and we tried to sort an already partitioned
        // sequence try to use insertion sort.
        if alreadyPartitioned
          and PartialInsertionSort(lo, pivotPos, size, compare)
          and PartialInsertionSort(pivotPos + size, hi, size, compare) then Exit;
      end;

      if (threads > 1) and ((leftSize > SequentialThreshold) or (rightSize > SequentialThreshold)) then
      begin
        PatternDefeatingQuickSort_Async(lo, pivotPos, hi, size, compare, threads shr 1, depthLimit, leftMost);
        Exit;
      end;

      PatternDefeatingQuickSort(lo, pivotPos, size, compare, 0, depthLimit, leftMost);
      lo := pivotPos + size;
      leftMost := False;
    end;
  until False;
end;

{$ENDREGION}


{$REGION 'TSort'}

class procedure TSort.SiftDown<T>(lo: Pointer<T>.Idx; hi, i: NativeUInt; const compare: TCompareMethod<T>);
var
  child: NativeUInt;
  temp: T;
begin
  child := i + i;
  if child < hi then
  repeat
    Inc(child);
    if child < hi then
      Inc(child, compare(lo^[child], lo^[child + 1]) shr 31);
    if compare(lo^[i], lo^[child]) >= 0 then
      Break;
    temp := lo^[i];
    lo^[i] := lo^[child];
    lo^[child] := temp;
    i := child;
    child := child + child;
  until child >= hi;
end;

class procedure TSort.HeapSort<T>(lo, hi: Pointer<T>.Idx; const compare: TCompareMethod<T>);
var
  len, i: NativeInt;
  temp: T;
begin
  len := (NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
  i := len shr 1;
  Dec(len);
  while i > 0 do
  begin
    Dec(i);
    SiftDown<T>(lo, len, i, compare);
  end;
  while len > 0 do
  begin
    temp := lo^[0];
    lo^[0] := lo^[len];
    lo^[len] := temp;
    Dec(len);
    SiftDown<T>(lo, len, 0, compare);
  end;
end;

class procedure TSort.InsertionSort<T>(lo, hi: Pointer<T>.Idx;
  const compare: TCompareMethod<T>);
var
  cur, sift: Pointer<T>.Idx;
  tmp: T;
begin
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    // Compare first so we can avoid 2 moves for an element already positioned correctly.
    if compare(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) < 0 then
    begin
      tmp := sift^[0];
      repeat
        sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
        Dec(sift);
      until (sift = lo) or (compare(tmp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) >= 0);
      sift^[0] := tmp;
    end;

    Inc(cur);
  until cur = hi;
end;

class procedure TSort.UnguardedInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TCompareMethod<T>);
var
  cur, sift: Pointer<T>.Idx;
  tmp: T;
begin
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    // Compare first so we can avoid 2 moves for an element already positioned correctly.
    if compare(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) < 0 then
    begin
      tmp := sift^[0];
      repeat
        sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
        Dec(sift);
      until compare(tmp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) >= 0;
      sift^[0] := tmp;
    end;

    Inc(cur);
  until cur = hi;
end;

class function TSort.PartialInsertionSort<T>(lo, hi: Pointer<T>.Idx;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): Boolean;
{$IFNDEF DELPHIXE6}
label
  ExitFalse;
{$ENDIF}
var
  limit: NativeInt;
  cur, sift: Pointer<T>.Idx;
  temp: T;
  comp: TCompareMethod<T>;
begin
  comp := compare;
  limit := 0;
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    if comp(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) >= 0 then
    begin
      Inc(cur);
      if cur <> hi then Continue;
      Break;
    end;

    temp := sift^[0];
    repeat
      sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
      Dec(sift);
    until (sift = lo) or (comp(temp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) >= 0);
    sift^[0] := temp;

    Inc(limit, (NativeUInt(cur) - NativeUInt(sift)) div NativeUInt(SizeOf(T)));
    if limit > PartialInsertionSortLimit then {$IFNDEF DELPHIXE6}goto ExitFalse{$ELSE}Exit(False){$ENDIF};
    Inc(cur);
    if cur = hi then Break;
  until False;
  Exit(True);
{$IFNDEF DELPHIXE6}
ExitFalse:
  Result := False;
{$ENDIF}
end;

class function TSort.PartitionLeft<T>(lo, hi: Pointer<T>.Idx;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): Pointer<T>.Idx;
var
  pivot, temp: T;
  first, last: Pointer<T>.Idx;
  comp: TCompareMethod<T>;
begin
  first := lo;
  last := hi;
  comp := compare;
  pivot := first^[0];

  repeat
    Dec(last);
  until comp(pivot, last^[0]) >= 0;

  if last + 1 = hi then
    repeat
      if first >= last then Break;
      Inc(first);
    until comp(pivot, first^[0]) < 0
  else
    repeat
      Inc(first);
    until comp(pivot, first^[0]) < 0;

  while first < last do
  begin
    temp := first^[0];
    first^[0] := last^[0];
    last^[0] := temp;
    repeat
      Dec(last);
    until comp(pivot, last^[0]) >= 0;
    repeat
      Inc(first)
    until comp(pivot, first^[0]) < 0;
  end;

  Result := last;
  lo^[0] := Result^[0];
  Result^[0] := pivot;
end;

class function TSort.PartitionRight<T>(lo, hi: Pointer<T>.Idx;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>;
  out pivotPos: Pointer<T>.Idx): Boolean;
var
  pivot, temp: T;
  first, last: Pointer<T>.Idx;
  comp: TCompareMethod<T>;
begin
  first := lo;
  last := hi;
  comp := compare;
  pivot := first^[0];

  // Find the first element greater than or equal than the pivot
  // (the median of 3 guarantees this exists).
  repeat
    Inc(first);
  until comp(first^[0], pivot) >= 0;

  // Find the first element strictly smaller than the pivot.
  // We have to guard this search if there was no element before first.
  if first - 1 = lo then
    repeat
      if first >= last then Break;
      Dec(last);
    until comp(last^[0], pivot) < 0
  else
    repeat
      Dec(last);
    until comp(last^[0], pivot) < 0;

  // If the first pair of elements that should be swapped to partition are the
  // same element, the passed in sequence already was correctly partitioned.
  if first >= last then
    Result := True

  // Keep swapping pairs of elements that are on the wrong side of the pivot.
  // Previously swapped pairs guard the searches, which is why the first
  // iteration is special-cased above.
  else
  begin
    repeat
      temp := first^[0];
      first^ := last^;
      last^[0] := temp;
      repeat
        Inc(first);
      until comp(first^[0], pivot) >= 0;
      repeat
        Dec(last);
      until comp(last^[0], pivot) < 0;
    until first >= last;
    Result := False;
  end;

  // Put the pivot in the right place.
  Dec(first);
  lo^ := first^;
  first^[0] := pivot;
  pivotPos := first;
end;

class procedure TSort.PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi: Pointer<T>.Idx;
  const compare: TCompareMethod<T>; threads, depthLimit: Integer; leftMost: Boolean);
var
  task: IInterface;
begin
  task := ParallelAPI.RunTask(
    procedure
    begin
      PatternDefeatingQuickSort<T>(lo, pivotPos, compare, threads, depthLimit, leftMost);
    end);
  PatternDefeatingQuickSort<T>(pivotPos + 1, hi, compare, threads, depthLimit, False);
  ParallelAPI.WaitFor(task);
end;

class procedure TSort.PatternDefeatingQuickSort<T>(lo, hi: Pointer<T>.Idx;
  const compare: TCompareMethod<T>; threads, depthLimit: Integer; leftMost: Boolean);
var
  partitionSize, leftSize, rightSize, i: NativeInt;
  alreadyPartitioned: Boolean;
  left, right, mid, pivotPos: Pointer<T>.Idx;
  temp: T;
begin
  if depthLimit < 0 then
  begin
    depthLimit := Log2((NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T)));
    if threads = 0 then
      threads := CPUCount;
  end;

  repeat
    partitionSize := (NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
    if partitionSize < InsertionSortThreshold then
    begin
      if not leftMost then
        UnguardedInsertionSort<T>(lo, hi, compare)
      else
        InsertionSort<T>(lo, hi, compare);
      Exit;
    end
    else
    begin
      left := lo;
      mid := lo + partitionSize shr 1;
      right := hi - 1;

      if compare(left^[0], mid^[0]) < 0 then
      begin
        temp := mid^[0];
        mid^[0] := left^[0];
        left^[0] := temp;
      end;
      if compare(right^[0], left^[0]) < 0 then
      begin
        temp := left^[0];
        left^[0] := right^[0];
        right^[0] := temp;
        if compare(left^[0], mid^[0]) < 0 then
        begin
          temp := mid^[0];
          mid^[0] := left^[0];
          left^[0] := temp;
        end;
      end;

      if not leftMost and (compare((lo - 1)^[0], lo^[0]) >= 0) then
      begin
        lo := PartitionLeft<T>(lo, hi, compare) + 1;
        Continue;
      end;

      // Partition and get results.
      alreadyPartitioned := PartitionRight<T>(lo, hi, compare, pivotPos);

      // Check for a highly unbalanced partition.
      leftSize := (NativeUInt(pivotPos) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
      rightSize := partitionSize - leftSize;

      // If we got a highly unbalanced partition we shuffle elements to break many patterns.
      if (leftSize < partitionSize shr 3) or (rightSize < partitionSize shr 3) then
      begin
        // If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
        Dec(depthLimit);
        if depthLimit = 0 then
        begin
          HeapSort<T>(lo, lo + partitionSize, compare);
          Exit;
        end;

        if leftSize >= InsertionSortThreshold then
        begin
          i := leftSize shr 2;
          left := lo;
          temp := left^[0];
          left^[0] := {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF} := temp;

          left := pivotPos - i;
          temp := {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF} := left^[0];
          left^[0] := temp;
        end;

        if rightSize >= InsertionSortThreshold then
        begin
          i := rightSize shr 2;
          left := pivotPos + 1;
          temp := left^[0];
          left^[0] := {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF} := temp;

          left := hi - i;
          temp := {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF} := left^[0];
          left^[0] := temp;
        end;
      end
      else
      begin
        // If we were decently balanced and we tried to sort an already partitioned
        // sequence try to use insertion sort.
        if alreadyPartitioned
          and PartialInsertionSort<T>(lo, pivotPos, compare)
          and PartialInsertionSort<T>(pivotPos + 1, hi, compare) then Exit;
      end;

      if (threads > 1) and ((leftSize > SequentialThreshold) or (rightSize > SequentialThreshold)) then
      begin
        PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi, compare, threads shr 1, depthLimit, leftMost);
        Exit;
      end;

      PatternDefeatingQuickSort<T>(lo, pivotPos, compare, threads, depthLimit, leftMost);
      lo := pivotPos + 1;
      leftMost := False;
    end;
  until False;
end;

class procedure TSort.SiftDown<T>(lo: Pointer<T>.Idx; hi, i: NativeUInt; const compare: TLessThanFunc<T>);
var
  child: NativeUInt;
  temp: T;
begin
  child := i + i;
  if child < hi then
  repeat
    Inc(child);
    if child < hi then
      Inc(child, Ord(compare(lo^[child], lo^[child + 1])));
    if not compare(lo^[i], lo^[child]) then
      Break;
    temp := lo^[i];
    lo^[i] := lo^[child];
    lo^[child] := temp;
    i := child;
    child := child + child;
  until child >= hi;
end;

class procedure TSort.HeapSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>);
var
  len, i: NativeInt;
  temp: T;
begin
  len := (NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
  i := len shr 1;
  Dec(len);
  while i > 0 do
  begin
    Dec(i);
    SiftDown<T>(lo, len, i, compare);
  end;
  while len > 0 do
  begin
    temp := lo^[0];
    lo^[0] := lo^[len];
    lo^[len] := temp;
    Dec(len);
    SiftDown<T>(lo, len, 0, compare);
  end;
end;

class procedure TSort.InsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>);
var
  cur, sift: Pointer<T>.Idx;
  tmp: T;
begin
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    // Compare first so we can avoid 2 moves for an element already positioned correctly.
    if compare(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) then
    begin
      tmp := sift^[0];
      repeat
        sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
        Dec(sift);
      until (sift = lo) or not compare(tmp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF});
      sift^[0] := tmp;
    end;

    Inc(cur);
  until cur = hi;
end;

class procedure TSort.UnguardedInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>);
var
  cur, sift: Pointer<T>.Idx;
  tmp: T;
begin
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    // Compare first so we can avoid 2 moves for an element already positioned correctly.
    if compare(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) then
    begin
      tmp := sift^[0];
      repeat
        sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
        Dec(sift);
      until not compare(tmp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF});
      sift^[0] := tmp;
    end;

    Inc(cur);
  until cur = hi;
end;

class function TSort.PartialInsertionSort<T>(lo, hi: Pointer<T>.Idx; const compare: TLessThanFunc<T>): Boolean;
{$IFNDEF DELPHIXE6}
label
  ExitFalse;
{$ENDIF}
var
  limit: NativeInt;
  cur, sift: Pointer<T>.Idx;
  temp: T;
  comp: TLessThanFunc<T>;
begin
  comp := compare;
  limit := 0;
  cur := lo + 1;
  if cur < hi then
  repeat
    sift := cur;

    if not comp(sift^[0], {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF}) then
    begin
      Inc(cur);
      if cur <> hi then Continue;
      Break;
    end;

    temp := sift^[0];
    repeat
      sift^[0] := {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF};
      Dec(sift);
    until (sift = lo) or not comp(temp, {$IFDEF POINTERMATH_BROKEN}(sift - 1)^[0]{$ELSE}Pointer<T>.Rec(sift)[-1].Value{$ENDIF});
    sift^[0] := temp;

    Inc(limit, (NativeUInt(cur) - NativeUInt(sift)) div NativeUInt(SizeOf(T)));
    if limit > PartialInsertionSortLimit then {$IFNDEF DELPHIXE6}goto ExitFalse{$ELSE}Exit(False){$ENDIF};
    Inc(cur);
    if cur = hi then Break;
  until False;
  Exit(True);
{$IFNDEF DELPHIXE6}
ExitFalse:
  Result := False;
{$ENDIF}
end;

class function TSort.PartitionLeft<T>(lo, hi: Pointer<T>.Idx;
  const compare: TLessThanFunc<T>): Pointer<T>.Idx;
var
  pivot, temp: T;
  first, last: Pointer<T>.Idx;
  comp: TLessThanFunc<T>;
begin
  first := lo;
  last := hi;
  comp := compare;
  pivot := first^[0];

  repeat
    Dec(last);
  until not comp(pivot, last^[0]);

  if last + 1 = hi then
    repeat
      if first >= last then Break;
      Inc(first);
    until comp(pivot, first^[0])
  else
    repeat
      Inc(first);
    until comp(pivot, first^[0]);

  while first < last do
  begin
    temp := first^[0];
    first^[0] := last^[0];
    last^[0] := temp;
    repeat
      Dec(last);
    until not comp(pivot, last^[0]);
    repeat
      Inc(first)
    until comp(pivot, first^[0]);
  end;

  Result := last;
  lo^[0] := Result^[0];
  Result^[0] := pivot;
end;

class function TSort.PartitionRight<T>(lo, hi: Pointer<T>.Idx;
  const compare: TLessThanFunc<T>; out pivotPos: Pointer<T>.Idx): Boolean;
var
  pivot, temp: T;
  first, last: Pointer<T>.Idx;
  comp: TLessThanFunc<T>;
begin
  first := lo;
  last := hi;
  comp := compare;
  pivot := first^[0];

  // Find the first element greater than or equal than the pivot
  // (the median of 3 guarantees this exists).
  repeat
    Inc(first);
  until not comp(first^[0], pivot);

  // Find the first element strictly smaller than the pivot.
  // We have to guard this search if there was no element before first.
  if first - 1 = lo then
    repeat
      if first >= last then Break;
      Dec(last);
    until comp(last^[0], pivot)
  else
    repeat
      Dec(last);
    until comp(last^[0], pivot);

  // If the first pair of elements that should be swapped to partition are the
  // same element, the passed in sequence already was correctly partitioned.
  if first >= last then
    Result := True

  // Keep swapping pairs of elements that are on the wrong side of the pivot.
  // Previously swapped pairs guard the searches, which is why the first
  // iteration is special-cased above.
  else
  begin
    repeat
      temp := first^[0];
      first^ := last^;
      last^[0] := temp;
      repeat
        Inc(first);
      until not comp(first^[0], pivot);
      repeat
        Dec(last);
      until comp(last^[0], pivot);
    until first >= last;
    Result := False;
  end;

  // Put the pivot in the right place.
  Dec(first);
  lo^ := first^;
  first^[0] := pivot;
  pivotPos := first;
end;

class procedure TSort.PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi: Pointer<T>.Idx;
  const compare: TLessThanFunc<T>; threads, depthLimit: Integer; leftMost: Boolean);
var
  task: IInterface;
begin
  task := ParallelAPI.RunTask(
    procedure
    begin
      PatternDefeatingQuickSort<T>(lo, pivotPos, compare, threads, depthLimit, leftMost);
    end);
  PatternDefeatingQuickSort<T>(pivotPos + 1, hi, compare, threads, depthLimit, False);
  ParallelAPI.WaitFor(task);
end;

class procedure TSort.PatternDefeatingQuickSort<T>(lo, hi: Pointer<T>.Idx;
  const compare: TLessThanFunc<T>; threads, depthLimit: Integer; leftMost: Boolean);
var
  partitionSize, leftSize, rightSize, i: NativeInt;
  alreadyPartitioned: Boolean;
  left, right, mid, pivotPos: Pointer<T>.Idx;
  temp: T;
begin
  if depthLimit < 0 then
  begin
    depthLimit := Log2((NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T)));
    if threads = 0 then
      threads := CPUCount;
  end;

  repeat
    partitionSize := (NativeUInt(hi) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
    if partitionSize < InsertionSortThreshold then
    begin
      if not leftMost then
        UnguardedInsertionSort<T>(lo, hi, compare)
      else
        InsertionSort<T>(lo, hi, compare);
      Exit;
    end
    else
    begin
      left := lo;
      mid := lo + partitionSize shr 1;
      right := hi - 1;

      if compare(left^[0], mid^[0]) then
      begin
        temp := mid^[0];
        mid^[0] := left^[0];
        left^[0] := temp;
      end;
      if compare(right^[0], left^[0]) then
      begin
        temp := left^[0];
        left^[0] := right^[0];
        right^[0] := temp;
        if compare(left^[0], mid^[0]) then
        begin
          temp := mid^[0];
          mid^[0] := left^[0];
          left^[0] := temp;
        end;
      end;

      if not leftMost and not compare((lo - 1)^[0], lo^[0]) then
      begin
        lo := PartitionLeft<T>(lo, hi, compare) + 1;
        Continue;
      end;

      // Partition and get results.
      alreadyPartitioned := PartitionRight<T>(lo, hi, compare, pivotPos);

      // Check for a highly unbalanced partition.
      leftSize := (NativeUInt(pivotPos) - NativeUInt(lo)) div NativeUInt(SizeOf(T));
      rightSize := partitionSize - leftSize;

      // If we got a highly unbalanced partition we shuffle elements to break many patterns.
      if (leftSize < partitionSize shr 3) or (rightSize < partitionSize shr 3) then
      begin
        // If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
        Dec(depthLimit);
        if depthLimit = 0 then
        begin
          HeapSort<T>(lo, lo + partitionSize, compare);
          Exit;
        end;

        if leftSize >= InsertionSortThreshold then
        begin
          i := leftSize shr 2;
          left := lo;
          temp := left^[0];
          left^[0] := {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF} := temp;

          left := pivotPos - i;
          temp := {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF} := left^[0];
          left^[0] := temp;
        end;

        if rightSize >= InsertionSortThreshold then
        begin
          i := rightSize shr 2;
          left := pivotPos + 1;
          temp := left^[0];
          left^[0] := {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i)^[0]{$ELSE}left^[i]{$ENDIF} := temp;

          left := hi - i;
          temp := {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF};
          {$IFDEF POINTERMATH_BROKEN}(left + i - 1)^[0]{$ELSE}left^[i - 1]{$ENDIF} := left^[0];
          left^[0] := temp;
        end;
      end
      else
      begin
        // If we were decently balanced and we tried to sort an already partitioned
        // sequence try to use insertion sort.
        if alreadyPartitioned
          and PartialInsertionSort<T>(lo, pivotPos, compare)
          and PartialInsertionSort<T>(pivotPos + 1, hi, compare) then Exit;
      end;

      if (threads > 1) and ((leftSize > SequentialThreshold) or (rightSize > SequentialThreshold)) then
      begin
        PatternDefeatingQuickSort_Async<T>(lo, pivotPos, hi, compare, threads shr 1, depthLimit, leftMost);
        Exit;
      end;

      PatternDefeatingQuickSort<T>(lo, pivotPos, compare, threads, depthLimit, leftMost);
      lo := pivotPos + 1;
      leftMost := False;
    end;
  until False;
end;

{$ENDREGION}


end.
