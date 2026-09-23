var
  len: NativeUInt;
  last: PDataType;
begin
  len := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
  if index < 0 then Exit;
  if count <= 0 then Exit;
  if NativeUInt(index) + NativeUInt(count) > len then Exit;

  last := @lo[index + count - 1];

  if NativeUInt(index) + NativeUInt(count) < len then
    QuickSelect(lo, hi, last);
  if index > 0 then
    QuickSelect(lo, last + 1, lo + index);
  if count <= InsertionSortThreshold then
    InsertionSort(lo + index, last + 1)
  else
    HeapSort(lo + index, last + 1);
end;
