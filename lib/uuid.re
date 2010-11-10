#
# uuid.re: (Almost) universally unique identifiers
# Beware, these may re-occur after 2^82 calls!  The horror!
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class UUID
  def initialize
    @ref = erl.make_ref()
  end
  
  def to_s()
    "#<UUID:#{erl.ref_to_list(@ref).to_string().sub('#Ref<', '')}"
  end
  
  def inspect(); to_s(); end
end
