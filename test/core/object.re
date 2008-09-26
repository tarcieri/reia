class PlusTwo
  def calc(n)
    n + 2

module ObjectTest
  def run
    Local.puts("Object")
    
    method_test()
  
  # implements method calls
  def method_test
    TestHelper.expect("implements method calls", fun do
      obj = PlusTwo.start()
      (44, obj.calc(42))
    )
      
  # reverses the order of a list
  def reverse_test
    TestHelper.expect("reverses the order of a list", fun do
      ([3,2,1].to_s(), [1,2,3].reverse().to_s())
    )