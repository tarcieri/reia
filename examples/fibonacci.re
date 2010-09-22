# Fibonacci numbers example
#
# From the toplevel Reia directory, run: bin/reia examples/fibonacci.re

module Fibonacci
  # Simple approach.  This example uses pattern matching and recursion,
  # although the tail call cannot be optimized in this case.
  def simple(0)
    0
  end
  def simple(1)
    1
  end
  def simple(n)
    simple(n - 1) + simple(n - 2)
  end
    
  # This example uses an optimized tail call, because the last thing it calls
  # is itself.  The compiler optimizes these cases into a "goto" call
  def optimized(n)
    optimized(n, 1, 0)
  end
  def optimized(0, _, result)
    result
  end
  def optimized(n, next, result)
    optimized(n - 1, next + result, next)
  end
    
  # Generate a list of Fibonacci numbers
  def list(n)
    [optimized(i) for i in 0..(n - 1)]
  end
    
  # Optimally generate a list of Fibonacci numbers
  def optimized_list(n)
    optimized_list(n - 1, 1, [0])
  end
  def optimized_list(0, _, result)
    result.reverse()
  end
  def optimized_list(n, next, result)
    optimized_list(n - 1, next + result[0], [next, *result])
  end
end
    
n = 42
"Fibonacci number #{n}: #{Fibonacci.optimized(n)}".puts()

n = 10
"First #{n} Fibonacci numbers: #{Fibonacci.optimized_list(n)}".puts()