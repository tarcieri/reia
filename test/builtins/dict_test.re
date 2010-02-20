#
# DictTest: Tests for Reia's dict type
# Copyright (C)2008-10 Tony Arcieri, Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module DictTest
  def run
    [
      get_test(),
      set_test(),
      compare_test(),
      to_s_test(),
      inspect_test(),
      size_test(),
      keys_test()
    ]
  end
    
  def get_test
    TestHelper.expect(Dict, "gets values by key") do
      ('bar', {:foo => 'bar'}[:foo])
    end
  end
  
  def set_test
    TestHelper.expect(Dict, "sets values of bound variables by key") do
      dict = {}
      dict[:foo] = 'bar'
      ('bar', dict[:foo])
    end
  end
  
  def compare_test
    TestHelper.expect(Dict, "compares dicts correctly") do
      ({:foo => 'foo', :bar => 'bar'}, {:bar => 'bar', :foo => 'foo'})
    end
  end
  
  def to_s_test
    TestHelper.expect(Dict, "casts to a string") do
      ("{:foo=>\"bar\"}",  {:foo => "bar"}.to_s())
    end
  end
  
  def inspect_test
    TestHelper.expect(Dict, "inspects properly") do
      ("{:foo=>\"bar\"}",  {:foo => "bar"}.inspect())
    end
  end
  
  def size_test
    TestHelper.expect(Dict, "calculates size") do
      (2, {:foo => "bar", :zoo => "horse"}.size())
    end
  end
  
  def keys_test
    TestHelper.expect(Dict, "retrieves keys as a list") do
      ({:foo => "bar", :zoo => "horse", :bar => "foo"}.keys(), [:bar, :zoo, :foo])
    end
  end
end