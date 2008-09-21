module TupleTest
  def run
    Local.puts("Tuple")
    
    nth_test()
    to_list_test()
    
  # retrieves the nth element correctly
  def nth_test
    Local.print("- retrieves the nth element correctly: ")
    
    tuple = (1,2,3,4,5)
    if tuple[0] == 1 and tuple[1] == 2 and tuple[4] == 5
      Local.puts("ok")
      true
    else
      Local.puts("FAILED")
      false
  
  # converts to a list
  def to_list_test
    TestHelper.expect("converts to a list", fun do
      ([1,2,3].to_s(), (1,2,3).to_list().to_s())
    )