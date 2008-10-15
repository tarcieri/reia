# Fibonacci numbers example
#
# From the toplevel Reia directory, run: bin/reia examples/fibonacci.re

module Fibonacci
  # Simple approach.  This example uses pattern matching and recursion,
  # although the tail call cannot be optimized in this case.
  def simple(0)
    0
  def simple(1)
    1
  def simple(n)
    simple(n - 1) + simple(n - 2)
    
  # This example uses an optimized tail call, because the last thing it calls
  # is itself.  The compiler optimizes these cases into a "goto" call
  def optimized(n)
    optimized(n, 1, 0)
  def optimized(0, _, result)
    result
  def optimized(n, next, result)
    optimized(n - 1, next + result, next)
    
n = 42
puts(["Fibonacci number ", n, ": ", Fibonacci.optimized(n)].join())