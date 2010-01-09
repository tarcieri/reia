#
# BranchingTest: Tests for Reia's branch expressions
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module BranchingTest
  def run
    [
      basic_if_test(),
      basic_if_nomatch_test(),
      basic_unless_test(),
      basic_unless_nomatch_test()
    ]
  end
  
  def basic_if_test
    TestHelper.expect("The 'if' statement", "takes true branches") do
      result = if true
        1
      end
      (1, result)
    end
  end
  
  def basic_if_nomatch_test
    TestHelper.expect("The 'if' statement", "returns nil if it didn't take a branch") do
      result = if false
        1
      end
      (nil, result)
    end
  end
  
  def basic_unless_test
    TestHelper.expect("The 'unless' statement", "takes false branches") do
      result = unless false
        1
      end
      (1, result)
    end
  end
  
  def basic_unless_nomatch_test
    TestHelper.expect("The 'if' statement", "returns nil if it didn't take a branch") do
      result = unless true
        1
      end
      (nil, result)
    end
  end
end