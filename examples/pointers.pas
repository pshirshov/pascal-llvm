program Pointers;

var
  x: integer;
  px: ^integer;

begin
  x := 42;
  px := @x;

  writeln('Value of x: ');
  writeln(x);

  writeln('Value through pointer: ');
  writeln(px^);

  px^ := 100;

  writeln('New value of x: ');
  writeln(x)
end.
