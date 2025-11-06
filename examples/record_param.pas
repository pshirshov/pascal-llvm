program RecordParam;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

def sum_point(p: Point): Integer = {
  return p.x + p.y
}

def main(): Integer = {
  var pt: Point
  pt.x = 15
  pt.y = 25
  var result: Integer = sum_point(pt)
  writeln(result)
  return 0
}

.
