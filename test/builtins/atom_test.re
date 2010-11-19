#
# atom_test.re: Tests for Reia's atom type
# Copyright (C)2008-10 Tony Arcieri, Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module AtomTest
  def run
    [to_s_test(), inspect_test()]
  end

  # converts to a string
  def to_s_test
    TestCase("Atom casts to a string") do |test|
      test.assert_equal("foo_bar", :'foo bar'.to_s())
    end
  end
  
  # inspects properly
  def inspect_test
    TestCase("Atom inspects properly") do |test|
      test.assert_equal(":'foo bar'", :'foo bar'.inspect())
    end
  end
end