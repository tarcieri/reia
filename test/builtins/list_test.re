#
# ListTest: Tests for Reia's list type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module ListTestHelper
  def some_list
    [1,3,5,7,9]
  end
end

module ListTest
  def run
    [
      size_test(), 
      nth_test(),
      replace_test(),
      complex_replace_test(),
      reverse_test(), 
      join_test(), 
      #parenless_sugar_test(), # FIXME this should work eventually...
      to_tuple_test(), 
      to_dict_test(),
      to_string_test(),
      to_s_test(),
      inspect_test()
    ]
  end
  
  # knows its size
  def size_test
    TestHelper.expect(List, "knows its size") do
      list = [1,2,3,4,5]
      (5, list.size())
    end
  end
  
  # retrieves the nth element correctly
  def nth_test
    TestHelper.expect(List, "retrieves the nth element correctly") do    
      list = [1,2,3,4,5]
      (true, list[0] == 1 and list[1] == 2 and list[4] == 5 and list[-1] == 5 and list[-3] == 3 and list[-10] == nil)
    end
  end
  
  # replaces elements in pattern matching expressions
  def replace_test
    TestHelper.expect(List, "replaces elements in pattern matching expressions ") do
      list = [1,2,3]
      list[1] = 42
      list[2] = 69
    
      ([1,42,69], list)
    end
  end
  
  # replaces elements in complex pattern matching expressions
  def complex_replace_test
    TestHelper.expect(List, "replaces elements in complex pattern matching expressions ") do
      list = [1,2,3,4]
      (list[1], [list[0], list[2]]) = (9, [8, 10])
    
      ([8,9,10,4], list)
    end
  end

  # reverses the order of a list
  def reverse_test
    TestHelper.expect(List, "reverses the order of a list") do
      ([3,2,1].to_s(), [1,2,3].reverse().to_s())
    end
  end

  # joins together into a string    
  def join_test
    TestHelper.expect(List, "joins into a string") do
      ("1,2,3", [1,2,3].join(','))
    end
  end
  
#  def parenless_sugar_test
#    TestHelper.expect(List, "members can be retrieved from function results without parens") do
#      (5, ListTestHelper.some_list[2])
#    end
#  end
    
  def to_tuple_test
    TestHelper.expect(List, "converts to a tuple") do
      ((1,2,3).to_s(), [1,2,3].to_tuple().to_s())
    end
  end
  
  def to_dict_test
    TestHelper.expect(List, "converts to a dict") do
      ([(:foo, "bar"), (:bar, "bar")].to_dict(), {:foo => "bar", :bar => "bar"})
    end
  end
  
  # converts to a string
  def to_string_test
    TestHelper.expect(List, "converts to a string") do
      ("surprise", [115,117,114,112,114,105,115,101].to_string())
    end
  end
  
  # casts to a string
  def to_s_test
    TestHelper.expect(Binary, "casts to a string") do
      ("[115,117,114,112,114,105,115,101]", [115,117,114,112,114,105,115,101].to_s())
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Binary, "inspects properly") do
      ("[115,117,114,112,114,105,115,101]", [115,117,114,112,114,105,115,101].inspect())
    end
  end
end