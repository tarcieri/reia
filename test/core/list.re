module ListTest
  def run
    Local.puts("List")
    
    nth_test()
    reverse_test()  
    push_test()
    pop_test()
    unshift_test()
    join_test()
    
  # retrieves the nth element correctly
  def nth_test
    Local.print("- retrieves the nth element correctly: ")
    
    list = [1,2,3,4,5]
    if list[0] == 1 and list[1] == 2 and list[4] == 5
      Local.puts("ok")
    else
      Local.puts("FAILED")
    
  # reverses the order of a list
  def reverse_test
    assert_equal("reverses the order of a list", [3,2,1], [1,2,3].reverse())
    
  def push_test
    assert_equal("appends elements with push", [1,2,3,4], [1,2,3].push(4))
    
  def pop_test
    assert_equal("removes last element with pop", 3, [1,2,3].pop())
    
  def unshift_test
    assert_equal("prepends elements with unshift", [1,2,3,4], [2,3,4].unshift(1))
    
  def shift_test
    assert_equal("removes first element with shift", 1, [1,2,3])
    
  def join_test
    assert_equal("joins into a string", "1,2,3", [1,2,3].join(','))
    
  def assert_equal(reason, expected, actual)
    io::format("- ~s: ".to_list(), [reason.to_list()])
    if expected.to_s() == actual.to_s()
      Local.puts("ok")
    else
      Local.puts("FAILED")
      io::format("  expected: ~s, actual: ~s~n".to_list(), [expected.to_s().to_list(), actual.to_s().to_list()])
    
    