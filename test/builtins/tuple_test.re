#
# TupleTest: Tests for Reia's tuple type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module TupleTest
  def run
    [
      size_test(), 
      nth_test(),
      replace_test(),
      complex_replace_test(),
      to_list_test(), 
      inspect_test()
    ]
  end

  # knows its size
  def size_test
    TestHelper.expect(Tuple, "knows its size") do
      list = (1,2,3,4,5)
      (5, list.size())
    end
  end
            
  # retrieves the nth element correctly
  def nth_test
    TestHelper.expect(Tuple, "retrieves the nth element correctly") do    
      tuple = (1,2,3,4,5)
      (true, tuple[0] == 1 and tuple[1] == 2 and tuple[4] == 5 and tuple[-2] == 4 and tuple[10] == nil)
    end
  end
  
  # replaces elements in pattern matching expressions
  def replace_test
    TestHelper.expect(Tuple, "replaces elements in pattern matching expressions ") do
      tuple = (1,2,3)
      tuple[1] = 42
      tuple[2] = 69
    
      ((1,42,69), tuple)
    end
  end
  
  # replaces elements in complex pattern matching expressions
  def complex_replace_test
    TestHelper.expect(Tuple, "replaces elements in complex pattern matching expressions ") do
      tuple = (1,2,3,4)
      (tuple[1], [tuple[0], tuple[2]]) = (9, [8, 10])
    
      ((8,9,10,4), tuple)
    end
  end
      
  # converts to a list
  def to_list_test
    TestHelper.expect(Tuple, "converts to a list") do
      ([1,2,3].to_s(), (1,2,3).to_list().to_s())
    end
  end
  
  def inspect_test
    TestHelper.expect(Tuple, "inspect returns valid tuple syntax") do
      (true, ().inspect() == "()" and (1,).inspect() == "(1,)" and (1,2).inspect() == "(1,2)")
    end
  end
end