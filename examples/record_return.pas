program RecordReturn;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

def make_point(a: Integer, b: Integer): Point = {
  var p: Point
  p.x = a
  p.y = b
  return p
}

def main(): Integer = {
  var pt: Point = make_point(100, 200)
  writeln(pt.x)
  writeln(pt.y)
  return 0
}

.
