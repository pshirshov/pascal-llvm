program LoopsTest;

def sum_to_n(n: Integer): Integer = {
  var total: Integer = 0
  for (i = 1 to n) {
    total := total + i
  }
  return total
}

def main(): Integer = {
  var result: Integer = sum_to_n(10)
  writeln(result)
  return 0
}

.
