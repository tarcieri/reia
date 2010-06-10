#
# numeric.re: Methods of the Numeric builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Numeric
  def to_s
    list = if erl.is_integer(self)
      erl.integer_to_list(self)
    elseif erl.is_float(self)
      erl.io_lib.format("~f".to_list(), [self])
    end
    
    list.to_string()
  end
  
  def inspect
    to_s()
  end
  
  def times(&block)
    erl.lists.foreach(block, erl.lists.seq(0, self - 1))
    self
  end
end
