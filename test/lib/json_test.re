#
# JsonTest: Tests for Reia's JSON parser
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module JsonTest
  def run
    [
      list_test(),
      object_test()
    ]
  end
  
  # parses lists
  def list_test
    TestHelper.expect("JSON", "parses lists") do
      ([1,2,3], "[1,2,3]".parse(:json))
    end
  end
  
  # parses objects
  def object_test
    TestHelper.expect("JSON", "parses objects") do
      ({'foo' => [1,2,3], 'bar' => [4,5,6]}, '{"foo": [1,2,3], "bar": [4,5,6]}'.parse(:json))
    end
  end
end