module ListTest
  def run
    Local.puts("List")
    
    nth_test()
    reverse_test()  
    push_test()
    pop_test()
    unshift_test()
    join_test()
    to_tuple_test()
    
  # retrieves the nth element correctly
  def nth_test
    Local.print("- retrieves the nth element correctly: ")
    
    list = [1,2,3,4,5]
    if list[0] == 1 and list[1] == 2 and list[4] == 5
      Local.puts("ok")
      true
    else
      Local.puts("FAILED")
      false
    
  # reverses the order of a list
  def reverse_test
    TestHelper.assert_equal("reverses the order of a list", [3,2,1].to_s(), [1,2,3].reverse().to_s())
    
  def push_test
    TestHelper.assert_equal("appends elements with push", [1,2,3,4].to_s(), [1,2,3].push(4).to_s())
    
  def pop_test
    TestHelper.assert_equal("removes last element with pop", 3, [1,2,3].pop())
    
  def unshift_test
    TestHelper.assert_equal("prepends elements with unshift", [1,2,3,4].to_s(), [2,3,4].unshift(1).to_s())
    
  def shift_test
    TestHelper.assert_equal("removes first element with shift", 1, [1,2,3].shift())
    
  def join_test
    TestHelper.assert_equal("joins into a string", "1,2,3", [1,2,3].join(','))
    
  def to_tuple_test()
    TestHelper.assert_equal("converts to a tuple", (1,2,3).to_s(), [1,2,3].to_tuple().to_s())
    
    