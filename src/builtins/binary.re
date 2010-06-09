#
# binary.re: Methods of the Binary builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Binary
  def call(fake_self, :to_string, args, block)
    # Define a new string via the internal representation
    (:reia_string, fake_self)
  end
  
  def call(fake_self, :to_s, args, block)
    fake_self.to_string()
  end
  
  def call(fake_self, :inspect, args, block)
    "<[#{erl.binary_to_list(fake_self).join(',')}]>"
  end
  
  def call(fake_self, :size, args, block)
    erl.byte_size(fake_self)
  end
end
