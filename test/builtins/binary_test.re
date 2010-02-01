#
# binary_test.re: Tests for Reia's atom type
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module BinaryTest
  def run
    [to_string_test(), to_s_test(), inspect_test()]
  end

  # converts to a string
  def to_string_test
    TestHelper.expect(Binary, "converts to a string") do
      ("foobar", <["foobar"]>.to_string())
    end
  end
  
  # casts to a string
  # converts to a string
  def to_s_test
    TestHelper.expect(Binary, "casts to a string") do
      ("foobar", <["foobar"]>.to_s())
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Binary, "inspects properly") do
      ("<[1,2,3]>", <[1,2,3]>.inspect())
    end
  end
end