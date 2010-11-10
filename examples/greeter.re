# Greeter example, showing simple use of classes
#
# From the toplevel Reia directory, run: bin/reia examples/greeter.re

class Greeter
  def initialize(greeting)
    @greeting = greeting
  end
  
  def greet(subject)
    "#{@greeting}, #{subject}!\n".print()
  end
end

greeter = Greeter("Hello")
greeter.greet("world") # prints: "Hello, world!"