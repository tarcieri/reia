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
    
  # Generate a list of Fibonacci numbers
  def list(n)
    [optimized(i) | i in 0..(n - 1)]
    
  # Optimally generate a list of Fibonacci numbers
  def optimized_list(n)
    optimized_list(n - 1, 1, [0])
  def optimized_list(0, _, result)
    result.reverse()
  def optimized_list(n, next, result)
    optimized_list(n - 1, next + result[0], result.unshift(next))
    
n = 42
puts(["Fibonacci number ", n, ": ", Fibonacci.optimized(n)].join())

n = 10
puts(["First ", n, " Fibonacci numbers: ", Fibonacci.optimized_list(n)].join())