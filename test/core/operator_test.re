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
      exponent_test(),
      bitwise_and_test(),
      bitwise_or_test(),
      bitwise_xor_test(),
      bitwise_not_test(),
      bitwise_lshift_test(),
      bitwise_rshift_test(),
      plus_equals_test(),
      minus_equals_test(),
      times_equals_test(),
      div_equals_test(),
      exponent_equals_test()
    ]
  end

  def plus_test
    TestHelper.expect("Operator", "+ adds") do
      (42, 41 + 1)
    end
  end
  
  def minus_test
    TestHelper.expect("Operator", "- subtracts") do
      (42, 43 - 1)
    end
  end
  
  def times_test
    TestHelper.expect("Operator", "* multiplies") do
      (42, 21 * 2)
    end
  end
  
  def div_test
    TestHelper.expect("Operator", "/ divides") do
      (42, 84 / 2)
    end
  end
  
  def exponent_test
    TestHelper.expect("Operator", "** exponentizes") do
      (42, (2 ** 5.39231749).to_i())
    end
  end
  
  def bitwise_and_test
    TestHelper.expect("Operator", "& bitwise ands") do
      (42, 43 & 46)
    end
  end  
  
  def bitwise_or_test
    TestHelper.expect("Operator", "| bitwise ors") do
      (42, 40 | 2)
    end
  end
  
  def bitwise_xor_test
    TestHelper.expect("Operator", "^ bitwise ors") do
      (42, 2 ^ 40)
    end
  end
  
  def bitwise_not_test
    TestHelper.expect("Operator", "~ bitwise nots") do
      (-44, ~43)
    end
  end
  
  def bitwise_lshift_test
    TestHelper.expect("Operator", "<< bitwise left shifts") do
      (420, 105 << 2)
    end
  end
  
  def bitwise_rshift_test
    TestHelper.expect("Operator", ">> bitwise right shifts") do
      (42, 168 >> 2)
    end
  end
  
  def plus_equals_test
    TestHelper.expect("Operator", "+= adds in place") do
      n = 41
      n += 1
      (42, n)
    end
  end
  
  def minus_equals_test
    TestHelper.expect("Operator", "-= subtracts in place") do
      n = 43
      n -= 1
      (42, n)
    end
  end
  
  def times_equals_test
    TestHelper.expect("Operator", "*= multiplies in place") do
      n = 21
      n *= 2
      (42, n)
    end
  end
  
  def div_equals_test
    TestHelper.expect("Operator", "/= divides in place") do
      n = 84
      n /= 2
      (42, n)
    end
  end
  
  def exponent_equals_test
    TestHelper.expect("Operator", "**= exponentizes in place") do
      n = 2
      n **= 5.39231749
      (42, n.to_i())
    end
  end
end
