#
# object_test.re: Tests for Reia's immutable objects
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Roflcopter
  def initialize(value)
    @lollerskate = value
  end
  
  def lollerskate
    @lollerskate
  end
end

module ObjectTest
  def run
    [instantiation_test(), method_test()]
  end
  
  def instantiation_test
    TestHelper.expect("Classes", "instantiate new objects") do
      Roflcopter(42)
      (true, true) # FIXME: this should be a real assertion
    end
  end
  
  def method_test
    TestHelper.expect("Methods", "can be invoked on objects") do
      obj = Roflcopter(42)
      (42, obj.lollerskate())
    end
  end
end