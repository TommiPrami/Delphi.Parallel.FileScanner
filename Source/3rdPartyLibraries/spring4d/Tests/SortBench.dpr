program SortBench;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Threading,
  Spring,
  Spring.Comparers,
  Spring.Benchmark,
  Spring.Threading,
  System.SysUtils,
  System.TypInfo;

type
  TTestRec = record
    value: Integer;
    text: string;
  end;

  TTestType = Integer;//TTestRec;

  TDistFunc = function(size: Integer): TArray<TTestType>;
  TSortFunc = procedure(var values: array of TTestType);

  IComparer = record
    VTable,
    QueryInterface,
    AddRef,
    Release,
    Compare: Pointer;
  end;

function Compare_TestType(inst: Pointer; const left, right: TTestRec): Integer;
//asm
//  xor     eax, eax
//  mov     edx, [edx]
//  cmp     edx, [ecx]
//  mov     edx, -1
//  setg    al
//  cmovl   eax, edx

begin
  if left.value < right.value then
    Result := -1
  else if left.value > right.value then
    Result := 1
  else
    Result := 0;
end;

const
  Comparer_TestType: IComparer = (
    VTable: @Comparer_TestType.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_TestType;
  );


function IntToTestType(i: Integer): TTestType;
begin
  Result := i;
end;

function Equals(const left, right: TTestType): Boolean; inline;
begin
  Result := left = right;
end;

function Shuffled(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(i);
  Spring.TArray.Shuffle<TTestType>(Result);
end;

function Shuffled_16Values(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(Random(16));
end;

function TwoValues(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(Random(2));
end;

function AllEqual(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(0);
end;

function Ascending(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(i);
end;

function SlightlyScrambled(size: Integer): TArray<TTestType>;
const
  scramblePercentage = 5;
var
  i, idx1, idx2: Integer;
  temp: TTestType;
  scrambleCount: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(i);
  scrambleCount := size * scramblePercentage div 100;
  for i := 1 to scrambleCount do
  begin
    idx1 := Random(size);
    idx2 := Random(size);
    temp := Result[idx1];
    Result[idx1] := Result[idx2];
    Result[idx2] := temp;
  end;
end;

function SortedBlocks(size: Integer): TArray<TTestType>;
type
  TBlock100 = array[1..100] of TTestType;
  TBlock10  = array[1..10] of TTestType;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := IntToTestType(i);
  if size > 100 then
    Spring.TArray.Shuffle<TBlock100>(Pointer(Result), (size div (SizeOf(TBlock100) div SizeOf(TTestType))) - 1)
  else
    Spring.TArray.Shuffle<TBlock10>(Pointer(Result), (size div (SizeOf(TBlock10) div SizeOf(TTestType))) - 1);
end;

function SingleBigSwap(size: Integer): TArray<TTestType>;
var
  i: Integer;
  halfSize: Integer;
begin
  SetLength(Result, size);
  halfSize := size div 2;
  for i := 0 to halfSize - 1 do
    Result[i] := IntToTestType(i + halfSize);
  for i := halfSize to size -1 do
    Result[i] := IntToTestType(i - halfSize);
end;

function Descending(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := size-1 downto 0 do
    Result[i] := IntToTestType(i);
end;

function PipeOrgan(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to (size div 2) - 1 do
    Result[i] := IntToTestType(i);
  for i := size div 2 to size -1 do
    Result[i] := IntToTestType(size - 1);
end;

function PushFront(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 1 to size - 1 do
    Result[i - 1] := IntToTestType(i);
  Result[size - 1] := IntToTestType(0);
end;

function PushMiddle(size: Integer): TArray<TTestType>;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to (size div 2) - 1 do
    Result[i] := IntToTestType(i);
  for i := (size div 2) + 1 to size -1 do
    Result[i - 1] := IntToTestType(i);
  Result[size - 1] := IntToTestType(size div 2);
end;

procedure QuickSort(var values: array of TTestType);
begin
  System.Generics.Collections.TArray.Sort<TTestType>(values);
end;

procedure QuickSort_Parallel(var values: array of TTestType);
begin
  System.Threading.TParallelArray.Sort<TTestType>(values
    , Spring.Comparers.TComparer<TTestType>.Default
    );
end;

procedure QuickSort_SpringComparer(var values: array of TTestType);
begin
  System.Generics.Collections.TArray.Sort<TTestType>(values
    , Spring.Comparers.TComparer<TTestType>.Default
    );
end;

procedure IntroSort(var values: array of TTestType);
begin
  Spring.TArray.IntroSort<TTestType>(values);
end;

procedure PDQSort(var values: array of TTestType);
begin
  Spring.TArray.Sort<TTestType>(values, TComparer<TTestType>.Default);
end;

procedure PDQSort_NoComparer(var values: array of TTestType);
begin
  Spring.TArray.Sort<TTestType>(values);
end;

procedure PDQSort_Parallel(var values: array of TTestType);
begin
  Spring.TArray.SortParallel<TTestType>(values, TComparer<TTestType>.Default);
end;

procedure PDQSort_Parallel_NoComparer(var values: array of TTestType);
begin
  Spring.TArray.SortParallel<TTestType>(values);
end;


//procedure HeapSort(var values: array of TTestType);
//var
//  compare: TCompareMethodRef;
//begin
//  TMethod(compare).Code := @Compare_TestType;
//  TMethod(compare).Data := nil;
////  Spring.Sorting.HeapSort(PInt32(@values[0]), PInt32(@values[Length(values)]));
//  HeapSort_Ref(@values[0], High(values), compare, SizeOf(TTestType));
//end;

type
  TArrayHelper = class(Spring.TArray);

procedure HeapSortGen(var values: array of TTestType);
var
  compare: TArray.TCompareMethodRef;
begin
  TMethod(compare).Code := @Compare_TestType;
  TMethod(compare).Data := nil;
//  Spring.Sorting.HeapSort(PUInt32(@values[0]), PUInt32(@values[Length(values)]));
//  TArray.HeapSort<TTestType>(@values[0], @values[High(values)], compare);
  TArrayHelper.HeapSort_Ref(@values[0], High(values), compare, SizeOf(TTestType));
end;

type
  {$SCOPEDENUMS ON}
  TSortAlgo = (
    QuickSort,
    QuickSort_SpringComparer,
    QuickSort_Parallel,
    IntroSort,
    PDQSort,
    PDQSort_NoComparer,
    PDQSort_Parallel,
    PDQSort_Parallel_NoComparer
  );
  {$SCOPEDENUMS OFF}

const
  Distributions: array[0..11] of record name: string; func: TDistFunc end = (
    (name: 'random'; func: Shuffled),
    (name: 'few_unique'; func: Shuffled_16Values),
    (name: 'two_values'; func: TwoValues),
    (name: 'all_equal'; func: AllEqual),
    (name: 'ascending'; func: Ascending),
    (name: 'slightly_scrambled'; func: SlightlyScrambled),
    (name: 'sorted_blocks'; func: SortedBlocks),
    (name: 'single_big_swap'; func: SingleBigSwap),
    (name: 'descending'; func: Descending),
    (name: 'pipe_organ'; func: PipeOrgan),
    (name: 'push_front'; func: PushFront),
    (name: 'push_middle'; func: PushMiddle)
  );

  Sorts: array[TSortAlgo] of TSortFunc = (
    QuickSort
    ,QuickSort_SpringComparer
    ,QuickSort_Parallel
    ,IntroSort
    ,PDQSort
    ,PDQSort_NoComparer
    ,PDQSort_Parallel
    ,PDQSort_Parallel_NoComparer
//    ,(name: 'HeapSortGen'; func: HeapSortGen)
//    ,(name: 'HeapSort'; func: HeapSort)
  );

  Sizes: array of Integer = [100, 1000, 10000, 100000, 1000000];

procedure BenchSort(const state: TState);
begin
  var distIdx := state[0];
  var sortIdx := state[1];
  var size := state[2];

  var distribution: TDistFunc := Distributions[distIdx].func;
  var sort: TSortFunc := Sorts[TSortAlgo(sortIdx)];
  var data := distribution(size);
  var copy: TArray<TTestType>;
  SetLength(copy, size);

  for var _ in state do
  begin
    state.PauseTiming;
    if IsManagedType(TTestType) then
      MoveManaged(data, copy, TypeInfo(TTestType), size)
    else
      Move(data[0], copy[0], size * SizeOf(TTestType));

    state.ResumeTiming;
    sort(copy);
  end;
  QuickSort(data);
  for var i := 0 to high(copy) do
    Assert(Equals(copy[i], data[i]), Distributions[distIdx].Name + '/' + GetEnumName(TypeInfo(TSortAlgo), sortIdx) + '/' + size.ToString);
end;

procedure Main;
begin
  if PTypeInfo(TypeInfo(TTestType)).Kind = tkRecord then
    Spring.Comparers.RegisterComparer(giComparer, TypeInfo(TTestType), IInterface(@Comparer_TestType));

  benchmark_format_args := False;

  for var distIdx := Low(distributions) to High(Distributions) do
  for var size in Sizes do
  for var sortIdx := Low(Sorts) to High(Sorts) do
  begin
//    if size > 10000 then Continue;

    //if Distributions[distIdx].name <> 'few_unique' then Continue;

    if not (sortIdx in [
      {TSortAlgo.QuickSort, }
      TSortAlgo.PDQSort_NoComparer,
      TSortAlgo.PDQSort_Parallel_NoComparer
      ]) then Continue;

    // prevent stackoverflow of QuickSort_Parallel on particular data distribution
    if sortIdx = TSortAlgo.QuickSort_Parallel then
    begin
      if Distributions[distIdx].name = 'few_unique' then
      if size >{$IFDEF CPU64BITS}={$ENDIF} 100000 then Continue;

      if Distributions[distIdx].name = 'two_values' then
      if size >= 100000 then Continue;

      if Distributions[distIdx].name = 'all_equal' then
      if size >= 10000 then Continue;

      if Distributions[distIdx].name = 'pipe_organ' then
      if size >= 100000 then Continue;
    end;

    var bm := Benchmark(BenchSort, Distributions[distIdx].name + '/' + GetEnumName(TypeInfo(TSortAlgo), Ord(sortIdx)) + '/count:' + size.ToString)
      .Args([distIdx, Ord(sortIdx), size]);
    bm.Iterations(100);
//    bm.MeasureProcessCPUTime;
  end;
  Benchmark_Main;
end;

procedure Test;
var
  data: TArray<TTestType>;
  sw: TStopwatch;
  dist: TDistFunc;
const
  Len = 1000000;
begin
  RandSeed := 0;
  dist := TwoValues;

  data := dist(Len);
  sw := TStopwatch.StartNew;
  Spring.TArray.Sort<TTestType>(data);
  Writeln(sw.ElapsedMilliseconds);

  RandSeed := 0;
  data := dist(Len);
  sw := TStopwatch.StartNew;
  Spring.TArray.SortParallel<TTestType>(data);
  Writeln(sw.ElapsedMilliseconds);
end;

//{$DEFINE USE_SYSTEM_THREADING}

function System_Threading_RunTask(const taskProc: TProc): IInterface;
begin
  Result := System.Threading.TTask.Run(taskProc);
end;

procedure System_Threading_WaitFor(const task: IInterface);
begin
  System.Threading.TTask.WaitForAll([System.Threading.ITask(task)]);
end;

procedure SetupParallelAPI;
begin
  {$IFDEF USE_SYSTEM_THREADING}
  Writeln('Using System.Threading');
  ParallelAPI.RunTask := System_Threading_RunTask;
  ParallelAPI.WaitFor := System_Threading_WaitFor;
  {$ENDIF}
  Writeln;
end;

begin
  SetupParallelAPI;
  try
//    Test;
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

//  Readln;
end.
