#
# RebindingTest: Tests for Reia's shortcut rebinding operators
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module RebindingTest
  def run
    [
      plusequals_test(),
      minusequals_test(),
      timesequals_test(),
      divequals_test()
    ]
  end
  
  # implements +=
  def plusequals_test
    TestHelper.expect("Rebinding", "implements +=") do
      n = 41
      n += 1
      (42, n)
    end
  end
  
  # implements -=
  def minusequals_test
    TestHelper.expect("Rebinding", "implements -=") do
      n = 43
      n -= 1
      (42, n)
    end
  end
  
  # implements *=
  def timesequals_test
    TestHelper.expect("Rebinding", "implements *=") do
      n = 21
      n *= 2
      (42, n)
    end
  end
  
  # implements /=
  def divequals_test
    TestHelper.expect("Rebinding", "implements .=") do
      n = 84
      n /= 2
      (42, n)
    end
  end
end