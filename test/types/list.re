#
# ListTest: Tests for Reia's list type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module ListTest
  def run
    [size_test(), 
    nth_test(), 
    reverse_test(), 
    push_test(), 
    pop_test(), 
    unshift_test(), 
    join_test(), 
    to_tuple_test(), 
    to_hash_test()]
  end
  
  # knows its size
  def size_test
    TestHelper.expect("List", "knows its size", fun do
      list = [1,2,3,4,5]
      (5, list.size())
    end)
  end
  
  # retrieves the nth element correctly
  def nth_test
    TestHelper.expect("List", "retrieves the nth element correctly", fun do    
      list = [1,2,3,4,5]
      (true, list[0] == 1 and list[1] == 2 and list[4] == 5 and list[-1] == 5 and list[-3] == 3 and list[-10] == nil)
    end)
  end
      
  # reverses the order of a list
  def reverse_test
    TestHelper.expect("List", "reverses the order of a list", fun do
      ([3,2,1].to_s(), [1,2,3].reverse().to_s())
    end)
  end
    
  def push_test
    TestHelper.expect("List", "appends elements with push", fun do
      ([1,2,3,4].to_s(), [1,2,3].push(4).to_s())
    end)
  end
    
  def pop_test
    TestHelper.expect("List", "removes last element with pop", fun do 
      (3, [1,2,3].pop())
    end)
  end
    
  def unshift_test
    TestHelper.expect("List", "prepends elements with unshift", fun do
      ([1,2,3,4].to_s(), [2,3,4].unshift(1).to_s())
    end)
  end
    
  def shift_test
    TestHelper.expect("List", "removes first element with shift", fun do
      (1, [1,2,3].shift())
    end)
  end
    
  def join_test
    TestHelper.expect("List", "joins into a string", fun do
      ("1,2,3", [1,2,3].join(','))
    end)
  end
    
  def to_tuple_test()
    TestHelper.expect("List", "converts to a tuple", fun do
      ((1,2,3).to_s(), [1,2,3].to_tuple().to_s())
    end)
  end
  
  def to_hash_test()
    TestHelper.expect("List", "converts to a hash", fun do
      ([(~foo, "bar"), (~baz, "baz")].to_hash(), {~foo: "bar", ~baz: "baz"})
    end)
  end
end