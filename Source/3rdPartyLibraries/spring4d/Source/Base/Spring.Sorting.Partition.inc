var
  mid: PDataType;
  partitionSize: NativeUInt;
  tmp, pivotVal: TDataType;
begin
  partitionSize := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
  mid := lo + (partitionSize shr 1);
  Dec(hi);

  if lo^ > mid^ then
  begin
    tmp := lo^;
    lo^ := mid^;
    mid^ := tmp;
  end;
  if lo^ > hi^ then
  begin
    tmp := lo^;
    lo^ := hi^;
    hi^ := tmp;
  end;
  if mid^ > hi^ then
  begin
    tmp := mid^;
    mid^ := hi^;
    hi^ := tmp;
  end;
  pivotVal := mid^;

  repeat
    while lo^ < pivotVal do
      Inc(lo);
    while hi^ > pivotVal do
      Dec(hi);
    if lo <= hi then
    begin
      tmp := lo^;
      lo^ := hi^;
      hi^ := tmp;
      Inc(lo);
      Dec(hi);
    end;
  until lo > hi;

  left := hi;
  right := lo;
end;
