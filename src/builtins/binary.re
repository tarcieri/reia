#
# binary.re: Methods of the Binary builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Binary
  def class; Binary; end
  
  def to_string
    # Define a new string via the internal representation
    (:reia_string, self)
  end
  
  def to_s
    to_string()
  end
  
  def inspect
    "<[#{erl.binary_to_list(self).join(',')}]>"
  end
  
  def size
    erl.byte_size(self)
  end
end
