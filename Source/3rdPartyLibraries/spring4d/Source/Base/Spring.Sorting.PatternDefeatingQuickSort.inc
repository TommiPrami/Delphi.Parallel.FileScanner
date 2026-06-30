
  procedure PatternDefeatingQuickSort_Async(lo, pivotPos, hi: PDataType;
    threads, depthLimit: Integer; leftMost, branchless: Boolean);
  var
    task: IInterface;
  begin
    task := ParallelAPI.RunTask(
      procedure
      begin
        PatternDefeatingQuickSort(lo, pivotPos, threads, depthLimit, leftMost, branchless);
      end);
    PatternDefeatingQuickSort(pivotPos + 1, hi, threads, depthLimit, False, branchless);
    ParallelAPI.WaitFor(task);
  end;

const
  PartitionRightFuncs: array[Boolean] of function(lo, hi: PDataType; out pivotPos: PDataType): Boolean = (
    PartitionRight,
    PartitionRightBranchless
  );
var
  partitionSize, leftSize, rightSize, i: NativeInt;
  alreadyPartitioned: Boolean;
  left, right, mid, pivotPos: PDataType;
  temp, temp2: TDataType;
begin
  partitionSize := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
  if depthLimit < 0 then
  begin
    depthLimit := Log2(partitionSize);
    branchless := partitionSize > 1000;
    if threads = 0 then
      threads := CPUCount;
  end;

  repeat
    if partitionSize < InsertionSortThreshold then
    begin
      if not leftMost then
        UnguardedInsertionSort(lo, hi)
      else
        InsertionSort(lo, hi);
      Exit;
    end
    else
    begin
      left := lo;
      right := hi - 1;
      mid := lo + partitionSize shr 1;

      if partitionSize > NintherThreshold then
      begin
        temp := left^;
        temp2 := mid^;
        if temp2 < temp then
        begin
          left^ := temp2;
          temp2 := temp;
          mid^ := temp;
        end;
        temp := right^;
        if temp < temp2 then
        begin
          right^ := temp2;
          temp2 := left^;
          mid^ := temp;
          if temp < temp2 then
          begin
            left^ := temp;
            mid^ := temp2;
          end;
        end;

        Inc(left);
        Dec(mid);
        Dec(right);

        temp := left^;
        temp2 := mid^;
        if temp2 < temp then
        begin
          left^ := temp2;
          temp2 := temp;
          mid^ := temp;
        end;
        temp := right^;
        if temp < temp2 then
        begin
          right^ := temp2;
          temp2 := left^;
          mid^ := temp;
          if temp < temp2 then
          begin
            left^ := temp;
            mid^ := temp2;
          end;
        end;

        Inc(left);
        Inc(mid, 2);
        Dec(right);

        temp := left^;
        temp2 := mid^;
        if temp2 < temp then
        begin
          left^ := temp2;
          temp2 := temp;
          mid^ := temp;
        end;
        temp := right^;
        if temp < temp2 then
        begin
          right^ := temp2;
          temp2 := left^;
          mid^ := temp;
          if temp < temp2 then
          begin
            left^ := temp;
            mid^ := temp2;
          end;
        end;

        left := mid - 2;
        right := mid;
        Dec(mid);

        temp := left^;
        temp2 := mid^;
        if temp2 < temp then
        begin
          left^ := temp2;
          temp2 := temp;
          mid^ := temp;
        end;
        temp := right^;
        if temp < temp2 then
        begin
          right^ := temp2;
          temp2 := left^;
          mid^ := temp;
          if temp < temp2 then
          begin
            left^ := temp;
            mid^ := temp2;
          end;
        end;

        temp := lo^;
        lo^ := mid^;
        mid^ := temp;
      end
      else
      begin
        temp := left^;
        temp2 := mid^;
        if temp2 < temp then
        begin
          left^ := temp2;
          temp2 := temp;
          mid^ := temp;
        end;
        temp := right^;
        if temp < temp2 then
        begin
          right^ := temp2;
          temp2 := left^;
          mid^ := temp;
          if temp < temp2 then
          begin
            left^ := temp;
            mid^ := temp2;
          end;
        end;
      end;

      if not leftMost and (lo[-1] >= lo^) then
      begin
        lo := PartitionLeft(lo, hi) + 1;
        partitionSize := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
        Continue;
      end;

      // Partition and get results.
      alreadyPartitioned := PartitionRightFuncs[branchless](lo, hi, pivotPos);

      // Check for a highly unbalanced partition.
      leftSize := (NativeUInt(pivotPos) - NativeUInt(lo)) shr Shift;
      rightSize := partitionSize - leftSize;

      // If we got a highly unbalanced partition we shuffle elements to break many patterns.
      if (leftSize < partitionSize shr 3) or (rightSize < partitionSize shr 3) then
      begin
        // If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
        Dec(depthLimit);
        if depthLimit = 0 then
        begin
          HeapSort(lo, hi);
          Exit;
        end;

        if leftSize >= InsertionSortThreshold then
        begin
          i := leftSize shr 2;
          left := lo;
          temp := left^;
          left^ := left[i];
          left[i] := temp;

          left := pivotPos;
          Dec(left, i);
          temp := left[i-1];
          left[i-1] := left^;
          left^ := temp;

          if leftSize > NintherThreshold then
          begin
            left := lo + 1;
            temp := left^;
            left^ := left[i];
            left[i] := temp;

            Inc(left);
            temp := left^;
            left^ := left[i];
            left[i] := temp;

            left := pivotPos - i - 1;
            temp := left[i-1];
            left[i-1] := left^;
            left^ := temp;

            Dec(left);
            temp := left[i-1];
            left[i-1] := left^;
            left^ := temp;
          end;
        end;

        if rightSize >= InsertionSortThreshold then
        begin
          i := rightSize shr 2;
          left := pivotPos + 1;
          temp := left^;
          left^ := left[i];
          left[i] := temp;

          left := hi;
          left := left - i;
          temp := left[i-1];
          left[i-1] := left^;
          left^ := temp;

          if rightSize > NintherThreshold then
          begin
            temp := pivotPos[2];
            pivotPos[2] := pivotPos[i+2];
            pivotPos[i+2] := temp;

            temp := pivotPos[3];
            pivotPos[3] := pivotPos[i+3];
            pivotPos[i+3] := temp;

            temp := hi[-2];
            hi[-2] := hi[-i-1];
            hi[-i-1] := temp;

            temp := hi[-3];
            hi[-3] := hi[-i-2];
            hi[-i-2] := temp;
          end;
        end;
      end
      else
      begin
        // If we were decently balanced and we tried to sort an already partitioned
        // sequence try to use insertion sort.
        if alreadyPartitioned
          and PartialInsertionSort(lo, pivotPos)
          and PartialInsertionSort(pivotPos + 1, hi) then Exit;
      end;

      if (threads > 1) and ((leftSize > SequentialThreshold) or (rightSize > SequentialThreshold)) then
      begin
        PatternDefeatingQuickSort_Async(lo, pivotPos, hi, threads shr 1, depthLimit, leftMost, branchless);
        Exit;
      end;

      PatternDefeatingQuickSort(lo, pivotPos, threads, depthLimit, leftMost, branchless);
      lo := pivotPos + 1;
      leftMost := False;
      partitionSize := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
    end;
  until False;
end;
