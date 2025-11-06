program Fibonacci;

def fib(n: Integer): Integer = {
  if (n <= 1) {
    return n
  } else {
    return fib(n - 1) + fib(n - 2)
  }
}

def main(): Integer = {
  var value: Integer = fib(5)
  writeln(value)
  return 0
}

.
