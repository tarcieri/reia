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
    TestHelper.expect(Atom, "casts to a string") do
      ("foo bar", :'foo bar'.to_s())
    end
  end
  
  # inspects properly
  def inspect_test
    TestHelper.expect(Atom, "inspects properly") do
      (":'foo bar'", :'foo bar'.inspect())
    end
  end
end