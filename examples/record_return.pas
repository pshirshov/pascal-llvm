program RecordReturn;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

function make_point(a: Integer; b: Integer): Point;
var
  p: Point;
begin
  p.x := a;
  p.y := b;
  return p
end;

function main(): Integer;
var
  pt: Point;
begin
  pt := make_point(100, 200);
  writeln(pt.x);
  writeln(pt.y);
  return 0
end;

.
