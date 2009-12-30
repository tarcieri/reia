#
# MapTest: Tests for Reia's map type
# Copyright (C)2008 Jared Kuolt, Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module MapTest
  def run
    [
      index_test(), 
      compare_test(), 
      remove_test(), 
      size_test(), 
      keys_test(), 
      has_test(), 
      has_test2()
    ]
  end
    
  def index_test
    TestHelper.expect(Map, "index recieves proper value") do
      ({:foo => "bar"}[:foo], "bar")
    end
  end
  
  def compare_test
    TestHelper.expect(Map, "comparison is equal") do
      ({:foo => "bar"}, {:foo => "bar"})
    end
  end
  
  def remove_test
    TestHelper.expect(Map, "removes a value") do
      ({:foo => "bar", :zoo => "horse"}.remove(:zoo), {:foo => "bar"})
    end
  end
  
  def size_test
    TestHelper.expect(Map, "calculates size") do
      ({:foo => "bar", :zoo => "horse"}.size(), 2)
    end
  end
  
  def keys_test
    TestHelper.expect(Map, "collects keys into a list") do
      ({:foo => "bar", :zoo => "horse", :bar => "foo"}.keys(), [:bar, :zoo, :foo])
    end
  end
  
  def has_test
    TestHelper.expect(Map, "checks if a key is presented") do
      ({:foo => "bar", :zoo => "horse"}.has(:foo), true)
    end
  end
  
  def has_test2
    TestHelper.expect(Map, "checks if a key is missing") do
      ({:foo => "bar", :zoo => "horse"}.has(:bar), false)
    end
  end
end