#
# binary.re: Methods of the Binary builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Binary
  def call(self, :to_string, args, block)
    # Define a new string via the internal representation
    (:reia_string, self)
  end
  
  def call(self, :to_s, args, block)
    self.to_string()
  end
  
  def call(self, :inspect, args, block)
    "<[#{erl.binary_to_list(self).join(',')}]>"
  end
  
  def call(self, :size, args, block)
    erl.byte_size(self)
  end
end