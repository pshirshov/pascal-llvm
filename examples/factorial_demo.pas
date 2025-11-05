program FactorialDemo;

function factorial(n: integer): integer;
var
  result: integer;
begin
  if n <= 1 then begin
    result := 1
  end else begin
    result := n * factorial(n - 1)
  end;
  return result
end;

function main(): integer;
var
  value: integer;
begin
  value := factorial(5);
  writeln(value);
  return 0
end;

.
