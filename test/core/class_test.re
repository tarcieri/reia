#
# class_test.re: tests for Reia's classes
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class IHasSingleton
  def self.example1
  end
  
  class self
    def example2
    end
    
    def example3
    end
  end
end