program ExprTest;

function add(a: integer; b: integer): integer;
begin
  return a + b
end;

function test(): integer;
begin
  return add(1, 2) + add(3, 4)
end;

.
