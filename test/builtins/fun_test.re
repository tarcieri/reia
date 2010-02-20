#
# fun_test.re: Tests for Reia's lambda type
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module FunTest
  def run
    [to_s_test(), inspect_test()]
  end

  # converts to a string
  def to_s_test
    TestHelper.expect(Fun, "casts to a string") do
      fun() { 2 + 2 }.to_s()
      (true, true) # FIXME not a real test
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Fun, "inspects properly") do
      fun() { 2 + 2 }.inspect()
      (true, true) # FIXME not a real test
    end
  end
end