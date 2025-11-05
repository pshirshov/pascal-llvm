program OneCallExpr;

function add(a: integer; b: integer): integer;
begin
  return a + b
end;

function test(): integer;
begin
  return add(1, 2) + 10
end;

.
