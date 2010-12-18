#
# integer.re: Methods of the Integer builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Integer < Numeric
  def class; Integer; end
  
  def to_s
    erl.integer_to_list(self).to_string()
  end
    
  def times(&block)
    erl.lists.foreach(block, erl.lists.seq(0, self - 1))
    self
  end
  
  def to_integer; self; end
  
  def to_float; self * 1.0; end
end