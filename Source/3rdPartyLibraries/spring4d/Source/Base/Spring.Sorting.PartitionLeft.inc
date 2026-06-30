var
  first, last: PDataType;
  pivot, temp: TDataType;
begin
  first := lo;
  last := hi;
  pivot := first^;

  repeat
    Dec(last);
  until pivot >= last^;

  if last + 1 = hi then
    repeat
      if first >= last then Break;
      Inc(first);
    until pivot < first^
  else
    repeat
      Inc(first);
    until pivot < first^;

  while first < last do
  begin
    temp := first^;
    first^ := last^;
    last^ := temp;
    repeat
      Dec(last);
    until pivot >= last^;
    repeat
      Inc(first)
    until pivot < first^;
  end;

  Result := last;
  lo^ := Result^;
  Result^ := pivot;
end;
