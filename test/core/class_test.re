#
# class_test.re: tests for Reia's classes
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class IHasClassMethods
  def self.foo
    :class_method
  end
  
  def foo
    :instance_method
  end
end