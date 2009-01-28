#
# ObjectTest: Tests for Reia's object system
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class PlusTwo
  def calc(n)
    n + 2
    
class StateHolder
  def get_val
    @val
  
  def set_val(newval)
    @val = newval

class InitializeTest
  def initialize(a, b, c)
    (@a, @b, @c) = (a, b, c)

  def ivars
    (@c, @b, @a)
    
class LocalMethodTest
  def a(n)
    2 * b(n)
    
  def b(n)
    n + 1
    
  def c(n)
    d(n)
    @foo = @foo + 1
    
  def d(n)
    @foo = n
    
  def foo
    @foo
    
class TestAncestor
  def simple1(n)
    n + 1
    
  def ivars1
    @foo = 21
    
class InheritanceTest < TestAncestor
  def simple2(n)
    simple1(n) * 2
    
  def ivars2
    ivars1()
    @foo * 2

module ObjectTest
  def run
    [method_test(), 
    local_method_test(), 
    local_method_ivar_test(), 
    state_test(), 
    initialize_test(),
    inheritance_test(),
    inheritance_ivar_test()]
      
  # implements method calls
  def method_test
    TestHelper.expect(Object, "implements method calls", fun do
      obj = PlusTwo()
      (44, obj.calc(42))
    )
    
  # implements local method calls
  def local_method_test
    TestHelper.expect(Object, "implements local method calls", fun do
      obj = LocalMethodTest()
      (4, obj.a(1))
    )
    
  # allows instance variable access from local methods
  def local_method_ivar_test
    TestHelper.expect(Object, "allows instance variable access from local methods", fun do
      obj = LocalMethodTest()
      obj.c(42)
      (43, obj.foo())
    )
      
  # stores state in instance_variables
  def state_test
    TestHelper.expect(Object, "stores state in instance_variables", fun do
      obj = StateHolder()
      obj.set_val(42)
      (42, obj.get_val())
    )

  # passes arguments from start to initialize
  def initialize_test
    TestHelper.expect(Object, "passes arguments from start to initialize", fun do
      obj = InitializeTest(1, 2, 3)
      ((3, 2, 1), obj.ivars())
    )

  # supports simple inheritance
  def inheritance_test
    TestHelper.expect(Object, "supports simple inheritance", fun do
      obj = InheritanceTest()
      (42, obj.simple2(20))
    )
    
  # allows instance variable access from inherited methods
  def inheritance_ivar_test
    TestHelper.expect(Object, "allows instance variable access from inherited methods", fun do
      obj = InheritanceTest()
      (42, obj.ivar2())
    )