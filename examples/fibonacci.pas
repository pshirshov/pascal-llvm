program Fibonacci;

function fib(n: integer): integer;
var
  result: integer;
begin
  if n <= 1 then begin
    result := n
  end else begin
    result := fib(n - 1) + fib(n - 2)
  end;
  return result
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
