program FibSimple;

function fib(n: integer): integer;
begin
  if n <= 1 then begin
    return n
  end;
  return fib(n - 1) + fib(n - 2)
end;

function main(): integer;
var
  value: integer;
begin
  value := fib(5);
  writeln(value);
  return 0
end;

.
