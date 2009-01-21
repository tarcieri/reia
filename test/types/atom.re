#
# AtomTest: Tests for Reia's atom type
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module AtomTest
  def run
    [to_string_test(), inspect_test()]

  def to_string_test
    TestHelper.expect(Atom, "to_string test", fun do
      (~'foo bar'.to_s(), "foo bar")
    )

  def inspect_test
    TestHelper.expect(Atom, "inspect shows proper value", fun do
      (~'foo bar'.inspect(), "~'foo bar'")
    )