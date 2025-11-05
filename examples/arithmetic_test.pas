program ArithmeticTest;

function compute(): integer;
var
  a: integer;
  b: integer;
  c: integer;
begin
  a := 10;
  b := 5;
  c := (a + b) * 2 - 10;
  return c
end;

function main(): integer;
var
  result: integer;
begin
  result := compute();
  writeln(result);
  return 0
end;

.
