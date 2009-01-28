# Greeter example
# (Inspired by a similar example on ruby-lang.org
#
# From the toplevel Reia directory, run: bin/reia examples/greeter.re

# The Greeter class
class Greeter
  def initialize(name)
    @name = name.capitalize()

  def salute
    ["Hello ", @name, "!"].join().puts()

# Create a new object
g = Greeter("world")
g.salute()
