#
# DictTest: Tests for Reia's dict type
# Copyright (C)2008-10 Tony Arcieri, Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module DictTest
  def run
    [
      retrieve_test()
      #compare_test(), 
      #size_test(),
      #keys_test()
    ]
  end
    
  def retrieve_test
    TestHelper.expect(Dict, "retrieves values by key") do
      ('bar', {:foo => 'bar'}[:foo])
    end
  end
  
  def compare_test
    TestHelper.expect(Dict, "compares dicts correctly") do
      ({:foo => 'foo', :bar => 'bar'}, {:bar => 'bar', :foo => 'bar'})
    end
  end
  
  def size_test
    TestHelper.expect(Dict, "calculates size") do
      ({:foo => "bar", :zoo => "horse"}.size(), 2)
    end
  end
  
  def keys_test
    TestHelper.expect(Dict, "retrieves keys as a list") do
      ({:foo => "bar", :zoo => "horse", :bar => "foo"}.keys(), [:bar, :zoo, :foo])
    end
  end
end