# Inheritance and polymorphism example
#
# Inspired by:
# http://en.wikipedia.org/wiki/Polymorphism_in_object-oriented_programming
#
# From the toplevel Reia directory, run: bin/reia examples/inheritance.re

class Animal
  def initialize(name)
    @name = name
  def name
    @name

class Cat < Animal
  def talk
    'Meow!'

class Dog < Animal
  def talk
    'Woof! Woof!'

animals = [Cat('Missy'), Dog('Mr. Bojangles'), Dog('Lassie')]
animals.each do |animal|
  [animal.name(), ' the ', animal.class(), ': ', animal.talk()].join().puts()
