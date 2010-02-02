#
# boolean.re: Methods of the Boolean builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
  
module Boolean
  def call(self, :to_s, args, block)
    erl.atom_to_list(self).to_string()
  end
  
  def call(self, :inspect, args, block)
    erl.atom_to_list(self).to_string()
  end
end