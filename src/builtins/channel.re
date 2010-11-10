#
# channel.re: Methods of the Channel builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Channel
  def class; Channel; end
  
  def to_string
    "#<Channel:#{erl.port_to_list(self).to_string().sub('#Port<', '')}"
  end
    
  def to_s; to_string(); end  
  def inspect; to_s(); end
end
