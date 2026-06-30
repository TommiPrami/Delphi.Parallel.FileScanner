var
  cur, sift: PDataType;
  tmp: TDataType;
begin
  cur := lo;
  repeat
    Inc(cur);
    if cur >= hi then Break;

    tmp := cur^;
    if tmp < cur[-1] then
    begin
      sift := cur;
      repeat
        sift^ := sift[-1];
        Dec(sift);
      until (sift = lo) or (tmp >= sift[-1]);
      sift^ := tmp;
    end;
  until False;
end;
