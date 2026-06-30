var
  first, last, first2, last2, left, right, left2, right2, left3, right3: PDataType;
  i, n: NativeUInt;
  pivot, pivot2, temp, temp2: TDataType;
  offsetsL, offsetsR, offsets, offsetsL2, offsetsR2, offsetsL3, offsetsR3: PByte;
  num, num2, num3, numL, numR, startL, startR, numUnknown, leftSplit, rightSplit: NativeUInt;
  idx, idxL, idxR: NativeInt;
  offsetsLStorage, offsetsRStorage: array[0..Pred(BlockSize + CacheLineSize)] of Byte;
begin
  first := lo;
  last := hi;
  pivot := first^;

  first2 := first;
  last2 := last;
  pivot2 := pivot;
  repeat
    Inc(first2);
  until first2^ >= pivot2;

  if first2 - 1 = lo then
  begin
    repeat
      if last2 < first2 then Break;
      Dec(last2);
    until last2^ < pivot2;
  end
  else
  begin
    repeat
      Dec(last2)
    until last2^ < pivot2;
  end;
  first := first2;
  last := last2;

  if first2 >= last2 then
    Result := True
  else
  begin
    temp := first^;
    first^ := last^;
    last^ := temp;
    Inc(first);

    offsetsL := AlignCacheLine(@offsetsLStorage[0]);
    offsetsR := AlignCacheLine(@offsetsRStorage[0]);

    left := first;
    right := last;
    numL := 0;
    numR := 0;
    startL := 0;
    startR := 0;

    while first < last do
    begin
      first2 := first;
      last2 := last;
      numUnknown := (NativeUInt(last2) - NativeUInt(first2)) shr Shift;
      leftSplit := 0;
      if (numL or numR) = 0 then leftSplit := numUnknown shr 1
      else if numR <> 0 then leftSplit := numUnknown;
      rightSplit := 0;
      if numR = 0 then rightSplit := numUnknown - leftSplit;

      offsets := offsetsL + numL;
      pivot2 := pivot;
      if leftSplit >= BlockSize then
      begin
        i := 0;
        repeat
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[0] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[1] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[2] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[3] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[4] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[5] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[6] >= pivot2));
          offsets^ := i; Inc(i); Inc(offsets, Ord(first2[7] >= pivot2));
          Inc(first2, 8);
        until i >= BlockSize;
      end
      else
      begin
        i := 0;
        while i < leftSplit do
        begin
          offsets^ := i; Inc(offsets, Ord(first2[i] >= pivot2)); Inc(i);
        end;
        Inc(first2, leftSplit);
      end;
      first := first2;
      numL := offsets - offsetsL;

      offsets := offsetsR + numR;
      pivot2 := pivot;
      if rightSplit >= BlockSize then
      begin
        i := 0;
        repeat
          Dec(last2, 8);
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[7] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[6] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[5] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[4] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[3] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[2] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[1] < pivot2));
          Inc(i); offsets^ := i; Inc(offsets, Ord(last2[0] < pivot2));
        until i >= BlockSize;
      end
      else
      begin
        i := 0;
        while i < rightSplit do
        begin
          Inc(i); offsets^ := i; Dec(last2); Inc(offsets, Ord(last2^ < pivot2));
        end;
      end;
      last := last2;
      numR := offsets - offsetsR;

      num := numL;
      if num > numR then
        num := numR;

      if numL = numR then
      begin
        offsetsL2 := offsetsL + startL;
        offsetsR2 := offsetsR + startR;
        left2 := left;
        right2 := right;
        i := 0;
        num2 := num;
        while i < num2 do
        begin
          idxL := offsetsL2[i];
          idxR := -NativeInt(offsetsR2[i]);
          temp := left2[idxL];
          left2[idxL] := right2[idxR];
          right2[idxR] := temp;
          Inc(i);
        end;
      end else if numL > 0 then
      begin
        offsetsL3 := offsetsL + startL;
        offsetsR3 := offsetsR + startR;
        left3 := left;
        right3 := right;
        idxL := offsetsL3[0];
        idxR := -NativeInt(offsetsR3[0]);
        temp2 := left3[idxL];
        left3[idxL] := right3[idxR];
        n := 1;
        num3 := num;
        while n < num3 do
        begin
          idxL := offsetsL3[n];
          right3[idxR] := left3[idxL];
          idxR := -NativeInt(offsetsR3[n]);
          left3[idxL] := right3[idxR];
          Inc(n);
        end;
        right3[idxR] := temp2;
      end;

      Dec(numL, num);
      Dec(numR, num);
      Inc(startL, num);
      Inc(startR, num);

      if numL = 0 then
      begin
        startL := 0;
        left := first;
      end;
      if numR = 0 then
      begin
        startR := 0;
        right := last;
      end;
    end;

    num2 := numL;
    if num2 <> 0 then
    begin
      offsets := offsetsL + startL + num2;
      left2 := left;
      last2 := last;
      repeat
        Dec(offsets);
        idx := offsets^;
        temp := left2[idx];
        Dec(last2);
        left2[idx] := last2^;
        last2^ := temp;
        Dec(num2);
      until num2 = 0;
      first := last2;
    end;
    num2 := numR;
    if num2 <> 0 then
    begin
      offsets := offsetsR + startR + num2;
      right2 := right;
      first2 := first;
      repeat
        Dec(offsets);
        idx := -NativeInt(offsets^);
        temp := right2[idx];
        right2[idx] := first2^;
        first2^ := temp;
        Inc(first2);
        Dec(num2);
      until num2 = 0;
      first := first2;
    end;
    Result := False;
  end;

  first2 := first - 1;
  lo^ := first2^;
  first2^ := pivot;
  pivotPos := first2;
end;
