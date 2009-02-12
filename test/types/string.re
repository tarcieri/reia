#
# StringTest: Tests for Reia's string type
# Copyright (C)2009 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module StringTest
  def run
    [length_test(), inspect_test(), sub_test(), split_test()]

  def length_test
    TestHelper.expect("String", "knows its length", fun do
      (6, "foobar".length())
    )

  def inspect_test
    TestHelper.expect("String", "inspects properly", fun do
      ("\"foobar\"", "foobar".inspect())
    )
    
  def sub_test
    TestHelper.expect("String", "substitutes properly", fun do
      ("bazbar", "foobar".sub(/foo/, "baz"))
    )
    
  def split_test
    TestHelper.expect("String", "splits properly", fun do
      (["foo", "bar", "baz"], "foo    bar baz".split(/\s+/))
    )