program IfTest;

function test(n: integer): integer;
var
  result: integer;
begin
  if n > 5 then
    result := 10
  else
    result := 20;
  return result
end;

.
