program TwoCalls;

function add(a: integer; b: integer): integer;
begin
  return a + b
end;

function test(): integer;
var
  x: integer;
  y: integer;
begin
  x := add(1, 2);
  y := add(3, 4);
  return x + y
end;

.
