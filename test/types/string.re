#
# StringTest: Tests for Reia's string type
# Copyright (C)2009 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module StringTest
  def run
    [length_test(), 
    inspect_test(), 
    sub_test(),
    interpolation_test()]
    #split_test() -- requires Erlang R12B-5, which isn't generally available :/
  end
  
  def length_test
    TestHelper.expect("String", "knows its length", fun do
      (6, "foobar".length())
    end)
  end
  
  def inspect_test
    TestHelper.expect("String", "inspects properly", fun do
      ("\"foobar\"", "foobar".inspect())
    end)
  end
    
  def sub_test
    TestHelper.expect("String", "substitutes properly", fun do
      ("bazbar", "foobar".sub(/foo/, "baz"))
    end)
  end
    
  def interpolation_test
    (foo, bar) = (1, 2)
    TestHelper.expect("String", "interpolates nested Reia syntax", fun do
      ("foo: 1, bar: 2", "foo: #{foo}, bar: #{bar}")
    end)
  end
    
  def split_test
    TestHelper.expect("String", "splits properly", fun do
      (["foo", "bar", "baz"], "foo    bar baz".split(/\s+/))
    end)
  end
end