#
# object_test.re: Tests for Reia's immutable objects
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Roflcopter
end

module ObjectTest
  def run
    [instantiation_test()]
  end
  
  def instantiation_test
    TestHelper.expect("Classes", "instantiate new objects") do
      Roflcopter()
    end
  end
end