program LoopsTest;

function sum_to_n(n: integer): integer;
var
  i: integer;
  total: integer;
begin
  total := 0;
  for i := 1 to n do begin
    total := total + i
  end;
  return total
end;

function main(): integer;
var
  result: integer;
begin
  result := sum_to_n(10);
  writeln(result);
  return 0
end;

.
