program RecordAssign;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

def test(): Integer = {
  var p1: Point
  var p2: Point
  p1.x = 10
  p1.y = 20
  p2 = p1  // Record assignment - should copy all fields
  p2.x = 30  // Modify p2
  return p1.x + p1.y + p2.x + p2.y  // Should be 10 + 20 + 30 + 20 = 80
}

def main(): Integer = {
  var result: Integer = test()
  writeln(result)
  return 0
}

.
