program ArrayOfRecords;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

type
  PointArray = array[3] of Point;

def test(): Integer = {
  var points: PointArray
  points[0].x = 10
  points[0].y = 20
  points[1].x = 30
  points[1].y = 40
  points[2].x = 50
  points[2].y = 60
  return points[0].x + points[1].y + points[2].x  // 10 + 40 + 50 = 100
}

def main(): Integer = {
  var result: Integer = test()
  writeln(result)
  return 0
}

.
