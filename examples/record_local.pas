program RecordLocal;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

def test(): Integer = {
  var p: Point
  p.x = 10
  p.y = 20
  return p.x + p.y
}

def main(): Integer = {
  var result: Integer = test()
  writeln(result)
  return 0
}

.
