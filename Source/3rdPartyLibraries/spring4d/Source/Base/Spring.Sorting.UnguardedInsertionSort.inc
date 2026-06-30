var
  cur, sift: PDataType;
  tmp, tmp1: TDataType;
begin
  cur := lo;
  repeat
    Inc(cur);
    if cur >= hi then Break;

    tmp := cur^;
    tmp1 := cur[-1];
    if tmp < tmp1 then
    begin
      sift := cur;
      repeat
        sift^ := tmp1;
        tmp1 := sift[-2];
        Dec(sift);
      until tmp >= tmp1;
      sift^ := tmp;
    end;
  until False;
end;
