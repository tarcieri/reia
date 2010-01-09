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
      #div_test(),
      plusequals_test(),
      minusequals_test(),
      timesequals_test(),
      divequals_test()
    ]
  end

  # implements +
  def plus_test
    TestHelper.expect("Operator", "+ is implemented") do
      n = 41
      (42, n + 1)
    end
  end
  
  # implements -
  def minus_test
    TestHelper.expect("Operator", "- is implemented") do
      n = 43
      (42, n - 1)
    end
  end
  
  # implements *
  def times_test
    TestHelper.expect("Operator", "* is implemented") do
      n = 21
      (42, n * 2)
    end
  end
  
  # implements /
  # FIXME: won't parse???
  #def div_test
  #  TestHelper.expect("Operator", "/ is implemented") do
  #    n = 84
  #    (42, n / 2)
  #  end
  #end
  
  # implements +=
  def plusequals_test
    TestHelper.expect("Operator", "+= is implemented") do
      n = 41
      n += 1
      (42, n)
    end
  end
  
  # implements -=
  def minusequals_test
    TestHelper.expect("Operator", "-= is implemented") do
      n = 43
      n -= 1
      (42, n)
    end
  end
  
  # implements *=
  def timesequals_test
    TestHelper.expect("Operator", "*= is implemented") do
      n = 21
      n *= 2
      (42, n)
    end
  end
  
  # implements /=
  def divequals_test
    TestHelper.expect("Operator", ".= is implemented") do
      n = 84
      n /= 2
      (42, n)
    end
  end
end