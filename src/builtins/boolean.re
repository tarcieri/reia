#
# boolean.re: Methods of the Boolean builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
  
module Boolean
  def call(fake_self, :to_s, args, block)
    erl.atom_to_list(fake_self).to_string()
  end
  
  def call(fake_self, :inspect, args, block)
    erl.atom_to_list(fake_self).to_string()
  end
end
