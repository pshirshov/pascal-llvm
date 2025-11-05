program FactTest;

function factorial(n: integer): integer;
begin
  if n <= 1 then
    return 1;
  return n * factorial(n - 1)
end;

.