#
# boolean_test.re: Tests for Reia's atom type
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module BooleanTest
  def run
    [to_s_test(), inspect_test()]
  end
  
  # casts to a string
  def to_s_test
    TestHelper.expect(Boolean, "casts to a string") do
      (("true", "false", "nil"), (true.to_s(), false.to_s(), nil.to_s()))
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Boolean, "inspects properly") do
      (("true", "false", "nil"), (true.inspect(), false.inspect(), nil.inspect()))
    end
  end
end