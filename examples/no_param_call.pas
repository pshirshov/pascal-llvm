program NoParamCall;

function get_value(): integer;
begin
  return 42
end;

function test(): integer;
var
  x: integer;
begin
  x := get_value();
  return x
end;

.
