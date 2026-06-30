var
  first, last: PDataType;
  pivot, temp: TDataType;
begin
  first := lo;
  last := hi;
  pivot := first^;

  repeat
    Inc(first);
  until first^ >= pivot;

  if first - 1 = lo then
    repeat
      if first >= last then Break;
      Dec(last);
    until last^ < pivot
  else
  begin
    repeat
      Dec(last);
    until last^ < pivot;
  end;

  if first >= last then
    Result := True
  else
  begin
    repeat
      temp := first^;
      first^ := last^;
      last^ := temp;
      repeat
        Inc(first);
      until first^ >= pivot;
      repeat
        Dec(last);
      until last^ < pivot;
    until first >= last;
    Result := False;
  end;

  Dec(first);
  lo^ := first^;
  first^ := pivot;
  pivotPos := first;
end;
