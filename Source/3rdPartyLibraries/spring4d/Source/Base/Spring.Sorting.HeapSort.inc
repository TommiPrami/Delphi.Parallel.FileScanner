  procedure SiftDown(Start2: PDataType; const len: NativeUInt; i: NativeUInt);
  var
    left, right, greater: NativeUInt;
    temp: TDataType;
  begin
    repeat
      left := 2 * i + 1;
      right := 2 * i + 2;
      if (right < len) and (Start2[left] < Start2[right]) then
        greater := right
      else
        greater := left;
      if (greater >= len) or not (Start2[i] < Start2[greater]) then
        Break;
      temp := start2[i];
      start2[i] := start2[greater];
      start2[greater] := temp;
      i := greater;
    until False;
  end;

var
  len, i: NativeInt;
  temp: TDataType;
begin
  len := (NativeUInt(hi) - NativeUInt(lo)) shr Shift;
  for i := len shr 1 - 1 downto 0 do
    SiftDown(lo, len, i);
  for i := len - 1 downto 1 do
  begin
    temp := lo[0];
    lo[0] := lo[i];
    lo[i] := temp;
    SiftDown(lo, i, 0);
  end;
end;
