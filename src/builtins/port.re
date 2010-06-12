#
# pid.re: Methods of the Port builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Port
  def class; Port; end
  
  def to_string
    "#<Port:#{erl.port_to_list(self).to_string().sub('#Port<', '')}"
  end
    
  def to_s; to_string(); end  
  def inspect; to_s(); end
end
