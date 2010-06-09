#
# tuple.re: Methods of the Tuple builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Tuple
  def call(fake_self, :'[]', (index,), block)
    length = erl.tuple_size(fake_self)
    
    index += length if index < 0
    index += 1
    
    erl.element(index, fake_self) if index >= 1 and index <= length
  end
  
  def call(fake_self, :'[]=', (index, value), block)
    length = erl.tuple_size(fake_self)
    
    index += length if index < 0
    index += 1
    
    erl.setelement(index, fake_self, value) if index >= 1 and index <= length
  end
  
  def call(fake_self, :size, args, block)
    erl.tuple_size(fake_self)
  end
  
  def call(fake_self, :to_list, args, block)
    erl.tuple_to_list(fake_self)
  end
  
  def call(fake_self, :to_s, args, block)
    case fake_self.to_list()
    when [expr]
      "(#{expr.inspect()},)"
    when _
      "(#{fake_self.to_list().map { |elem| elem.inspect() }.join(',')})"
    end
  end
  
  def call(fake_self, :inspect, args, block)
    fake_self.to_s()
  end
end
