class PlusTwo
  def calc(n)
    n + 2
    
class StateHolder
  def get_val
    @val
  
  def set_val(newval)
    @val = newval

module ObjectTest
  def run
    Local.puts("Object")
    
    method_test()
    state_test()
  
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