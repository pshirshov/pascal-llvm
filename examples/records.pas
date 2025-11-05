program Records;

type
  Point = record
    x: integer;
    y: integer;
  end;

var
  p1: Point;
  p2: Point;

begin
  p1.x := 10;
  p1.y := 20;

  p2.x := 30;
  p2.y := 40;

  writeln('Point 1: (');
  write(p1.x);
  write(', ');
  write(p1.y);
  writeln(')');

  writeln('Point 2: (');
  write(p2.x);
  write(', ');
  write(p2.y);
  writeln(')')
end.
