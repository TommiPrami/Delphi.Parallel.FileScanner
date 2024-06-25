program Demo.Spring.Nullable;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uNullableDemo in 'uNullableDemo.pas';

begin
  try
    RunNullableDemo;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
