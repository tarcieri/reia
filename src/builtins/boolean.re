#
# boolean.re: Methods of the Boolean builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
  
class Boolean
  def class; Boolean; end
  
  def to_s
    erl.atom_to_list(self).to_string()
  end
  
  def inspect
    to_s()
  end
end
