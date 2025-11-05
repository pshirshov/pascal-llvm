program SimpleCall;

function add(a: integer; b: integer): integer;
begin
  return a + b
end;

function test(): integer;
var
  x: integer;
begin
  x := add(1, 2);
  return x
end;

.
