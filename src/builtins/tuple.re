#
# tuple.re: Methods of the Tuple builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Tuple
  def call(self, :'[]', (index,), block)
    length = erl.tuple_size(self)
    
    index += length if index < 0
    index += 1
    
    erl.element(index, self) if index >= 1 and index <= length
  end
  
  def call(self, :'[]=', (index, value), block)
    length = erl.tuple_size(self)
    
    index += length if index < 0
    index += 1
    
    erl.setelement(index, self, value) if index >= 1 and index <= length
  end
  
  def call(self, :size, args, block)
    erl.tuple_size(self)
  end
  
  def call(self, :to_list, args, block)
    erl.tuple_to_list(self)
  end
  
  def call(self, :to_s, args, block)
    case self.to_list()
    when [expr]
      "(#{expr.inspect()},)"
    when _
      "(#{self.to_list().map { |elem| elem.inspect() }.join(',')})"
    end
  end
  
  def call(self, :inspect, args, block)
    self.to_s()
  end
end