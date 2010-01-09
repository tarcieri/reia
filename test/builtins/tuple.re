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
      (true, tuple[0] == 1 and tuple[1] == 2 and tuple[4] == 5 and tuple[-2] == 4)
    end
  end
  
  # replaces elements in pattern matching expressions
  def replace_test
    tuple = (1,2,3)
    tuple[1] = 42
    tuple[2] = 69
    
    ((1,42,69), tuple)
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