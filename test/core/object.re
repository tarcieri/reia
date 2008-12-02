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

module ObjectTest
  def run
    Local.puts("Object")
    
    method_test()
    state_test()
    initialize_test()
  
  # implements method calls
  def method_test
    TestHelper.expect("implements method calls", fun do
      obj = PlusTwo.start()
      (44, obj.calc(42))
    )
      
  # stores state in instance_variables
  def state_test
    TestHelper.expect("stores state in instance_variables", fun do
      obj = StateHolder.start()
      obj.set_val(42)
      (42, obj.get_val())
    )

  # passes arguments from start to initialize
  def initialize_test
    TestHelper.expect("passes arguments from start to initialize", fun do
      obj = InitializeTest.start(1, 2, 3)
      ((3, 2, 1), obj.ivars())
    )
