#
# range_test.re: Tests for Reia's Range type
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module RangeTest
  def run
    [to_list_test(), to_s_test(), inspect_test()]
  end

  # converts to a list
  def to_list_test
    TestHelper.expect(Range, "converts to a list") do
      ([1,2,3,4,5], (1..5).to_list())
    end
  end
  
  # casts to a string
  def to_s_test
    TestHelper.expect(Range, "casts to a string") do
      ("1..5", (1..5).to_s())
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Range, "inspects properly") do
      ("1..5", (1..5).to_s())
    end
  end
end