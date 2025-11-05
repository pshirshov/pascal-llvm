program RecordParam;

type
  Point = record
    x: integer;
    y: integer;
  end;

function sum_point(p: Point): integer;
begin
  return p.x + p.y
end;

function main(): integer;
var
  pt: Point;
  result: integer;
begin
  pt.x := 15;
  pt.y := 25;
  result := sum_point(pt);
  writeln(result);
  return 0
end;

.
