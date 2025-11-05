program Factorial;

function factorial(n: integer): integer;
begin
  if n <= 1 then
    return 1
  else
    return n * factorial(n - 1)
end;

function main(): integer;
var
  num: integer;
  result: integer;
begin
  num := 5;
  result := factorial(num);
  writeln(result);
  return 0
end;

.
