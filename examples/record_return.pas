program RecordReturn;

type
  Point = record
    x: integer;
    y: integer;
  end;

function make_point(a: integer; b: integer): Point;
var
  p: Point;
begin
  p.x := a;
  p.y := b;
  return p
end;

function main(): integer;
var
  pt: Point;
begin
  pt := make_point(100, 200);
  writeln(pt.x);
  writeln(pt.y);
  return 0
end;

.
