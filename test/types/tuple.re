module TupleTest
  def run
    Local.puts("Tuple")
    
    nth_test()
    to_list_test()
    
  # retrieves the nth element correctly
  def nth_test
    TestHelper.expect("retrieves the nth element correctly", fun do    
      tuple = (1,2,3,4,5)
      (true, tuple[0] == 1 and tuple[1] == 2 and tuple[4] == 5 and tuple[-2] == 4)
    )
      
  # converts to a list
  def to_list_test
    TestHelper.expect("converts to a list", fun do
      ([1,2,3].to_s(), (1,2,3).to_list().to_s())
    )
