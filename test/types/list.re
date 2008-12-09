#
# ListTest: Tests for Reia's list type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module ListTest
  def run
    Local.puts("List")
    
    size_test()
    nth_test()
    reverse_test()  
    push_test()
    pop_test()
    unshift_test()
    join_test()
    to_tuple_test()
  
  # knows its size
  def size_test
    TestHelper.expect("knows its size", fun do
      list = [1,2,3,4,5]
      (5, list.size())
    )
  # retrieves the nth element correctly
  def nth_test
    TestHelper.expect("retrieves the nth element correctly", fun do    
      list = [1,2,3,4,5]
      (true, list[0] == 1 and list[1] == 2 and list[4] == 5 and list[-1] == 5 and list[-3] == 3 and list[-10] == nil)
    )
      
  # reverses the order of a list
  def reverse_test
    TestHelper.expect("reverses the order of a list", fun do
      ([3,2,1].to_s(), [1,2,3].reverse().to_s())
    )
    
  def push_test
    TestHelper.expect("appends elements with push", fun do
      ([1,2,3,4].to_s(), [1,2,3].push(4).to_s())
    )
    
  def pop_test
    TestHelper.expect("removes last element with pop", fun do 
      (3, [1,2,3].pop())
    )
    
  def unshift_test
    TestHelper.expect("prepends elements with unshift", fun do
      ([1,2,3,4].to_s(), [2,3,4].unshift(1).to_s())
    )
    
  def shift_test
    TestHelper.expect("removes first element with shift", fun do
      (1, [1,2,3].shift())
    )
    
  def join_test
    TestHelper.expect("joins into a string", fun do
      ("1,2,3", [1,2,3].join(','))
    )
    
  def to_tuple_test()
    TestHelper.expect("converts to a tuple", fun do
      ((1,2,3).to_s(), [1,2,3].to_tuple().to_s())
    )
