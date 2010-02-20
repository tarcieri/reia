# Inheritance and polymorphism example
#
# Inspired by:
# http://en.wikipedia.org/wiki/Polymorphism_in_object-oriented_programming
#
# From the toplevel Reia directory, run: bin/reia examples/inheritance.re

class Animal
  def initialize(name)
    @name = name
  end
  
  def name
    @name
  end
end

class Cat < Animal
  def talk
    'Meow!'
  end
end

class Dog < Animal
  def talk
    'Woof! Woof!'
  end
end

animals = [Cat('Missy'), Dog('Mr. Bojangles'), Dog('Lassie')]

animals.each do |animal|
  "#{animal.name()} the #{animal.class()} says: #{animal.talk()}".puts()
end