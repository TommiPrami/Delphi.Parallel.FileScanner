var
  limit: NativeInt;
  cur, sift: PDataType;
  tmp: TDataType;
begin
  limit := PartialInsertionSortLimit;
  cur := lo;
  repeat
    Inc(cur);
    if cur >= hi then Break;

    tmp := cur^;
    if tmp < cur[-1] then
    begin
      sift := cur;
      repeat
        Dec(limit);
        sift^ := sift[-1];
        Dec(sift);
      until (sift = lo) or (tmp >= sift[-1]);
      sift^ := tmp;

      if limit <= 0 then Exit(False);
    end;
  until False;
  Result := True;
end;
