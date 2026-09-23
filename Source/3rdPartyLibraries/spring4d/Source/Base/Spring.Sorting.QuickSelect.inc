var
  left, right: PDataType;
  partitionSize: NativeUInt;
begin
  repeat
    partitionSize := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
    if partitionSize <= InsertionSortThreshold then
    begin
      InsertionSort(lo, hi);
      Exit;
    end;

    Partition(lo, hi, left, right);

    if target <= left then
      hi := left + 1
    else if target >= right then
      lo := right
    else
      Exit;
  until False;
end;
