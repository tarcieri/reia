#
# uuid.re: (Almost) universally unique identifiers
# Beware, these may re-occur after 2^82 calls!  The horror!
# Copyright (C)2010-11 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class UUID
  # UUIDs are UUIDs!
  def class; UUID; end
    
  def to_s()
    "#<UUID:#{erl.ref_to_list(self).to_string().sub('#Ref<', '')}"
  end
  
  def inspect(); to_s(); end
end
