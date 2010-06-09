#
# numeric.re: Methods of the Numeric builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Numeric
  def call(fake_self, :to_s, args, block)
    list = if erl.is_integer(fake_self)
      erl.integer_to_list(fake_self)
    elseif erl.is_float(fake_self)
      erl.io_lib.format("~f".to_list(), [fake_self])
    end
    
    list.to_string()
  end
  
  def call(fake_self, :inspect, args, block)
    fake_self.to_s()
  end
  
  def call(fake_self, :times, args, block)
    erl.lists.foreach(block, erl.lists.seq(0, fake_self - 1))
    fake_self
  end
end
