program RecordParam;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

def sum_point(p: Point): Integer = {
  return p.x + p.y
}

function main(): Integer;
var
  pt: Point;
  result: Integer;
begin
  pt.x := 15;
  pt.y := 25;
  result := sum_point(pt);
  writeln(result);
  return 0
end;

.
