module ListTest
  def run
    Local.puts("List")
    
    Local.print("- retrieves the nth element correctly: ")
    Local.puts(nth_test().to_s())
    
    reverse_test()  
    push_test()
    
  # retrieves the nth element correctly
  def nth_test
    list = [1,2,3,4,5]
    list[0] == 1 and list[1] == 2 and list[4] == 5
    
  # reverses the order of a list
  def reverse_test
    assert_equal("reverses the order of a list", [3,2,1], [1,2,3].reverse())
    
  def push_test
    assert_equal("appends elements with push", [1,2,3,4], [1,2,3].push(4))
    
  def assert_equal(reason, expected, actual)
    io::format("- ~s: ".to_list(), [reason.to_list()])
    if expected.to_s() == actual.to_s()
      Local.puts("ok.")
    else
      Local.puts("FAILED")
      io::format("  expected: ~s, actual: ~s~n".to_list(), [expected.to_s().to_list(), actual.to_s().to_list()])
    
    