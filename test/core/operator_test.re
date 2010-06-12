#
# OperatorTest: Tests for Reia's various operators
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module OperatorTest
  def run
    [
      plus_test(),
      minus_test(),
      times_test(),
      div_test(),
      plusequals_test(),
      minusequals_test(),
      timesequals_test(),
      divequals_test()
    ]
  end

  def plus_test
    TestHelper.expect("Operator", "+ adds") do
      n = 41
      (42, n + 1)
    end
  end
  
  def minus_test
    TestHelper.expect("Operator", "- subtracts") do
      n = 43
      (42, n - 1)
    end
  end
  
  def times_test
    TestHelper.expect("Operator", "* multiplies") do
      n = 21
      (42, n * 2)
    end
  end
  
  def div_test
    TestHelper.expect("Operator", "/ divides") do
      n = 84
      (42, n / 2)
    end
  end
  
  def plusequals_test
    TestHelper.expect("Operator", "+= adds in place") do
      n = 41
      n += 1
      (42, n)
    end
  end
  
  def minusequals_test
    TestHelper.expect("Operator", "-= subtracts in place") do
      n = 43
      n -= 1
      (42, n)
    end
  end
  
  def timesequals_test
    TestHelper.expect("Operator", "*= multiplies in place") do
      n = 21
      n *= 2
      (42, n)
    end
  end
  
  def divequals_test
    TestHelper.expect("Operator", "/= divides in place") do
      n = 84
      n /= 2
      (42, n)
    end
  end
end