#
# tuple.re: Methods of the Tuple builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Tuple
  def class; Tuple; end
  
  def [](index)
    length = erl.tuple_size(self)
    
    index += length if index < 0
    index += 1
    
    erl.element(index, self) if index >= 1 and index <= length
  end
  
  def []=(index, value)
    length = erl.tuple_size(self)
    
    index += length if index < 0
    index += 1
    
    erl.setelement(index, self, value) if index >= 1 and index <= length
  end
  
  def size
    erl.tuple_size(self)
  end
  
  def to_list
    erl.tuple_to_list(self)
  end
  
  def to_s
    case self.to_list()
    when [expr]
      "(#{expr.inspect()},)"
    when _
      "(#{self.to_list().map { |elem| elem.inspect() }.join(',')})"
    end
  end
  
  def inspect
    self.to_s()
  end
end
