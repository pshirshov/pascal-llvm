program Arrays;

var
  numbers: array[10] of integer;
  i: integer;
  sum: integer;

begin
  sum := 0;

  // Initialize array
  for i := 0 to 9 do
  begin
    numbers[i] := i * 2
  end;

  // Calculate sum
  for i := 0 to 9 do
  begin
    sum := sum + numbers[i]
  end;

  writeln('Sum of array elements: ');
  writeln(sum)
end.
