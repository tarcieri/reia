#
# Numeric: Methods of the Numeric builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Numeric
  def call(self, :to_s, args, block)
    list = if erl.is_integer(self)
      erl.integer_to_list(self)
    elseif erl.is_float(self)
      erl.io_lib.format("~f".to_list(), [self])
    end
    
    list.to_string()
  end
  
  def call(self, :inspect, args, block)
    self.to_s()
  end
end