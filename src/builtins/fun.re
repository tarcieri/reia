#
# fun.re: Methods of the Fun builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Fun
  def call(fake_self, :inspect, args, block)
    (:name,   name) = erl.fun_info(fake_self, :name)
    (:module, mod)  = erl.fun_info(fake_self, :module)
    "#<Fun #{name}:#{mod}>"
  end
  
  def call(fake_self, :to_s, args, block)
    call(fake_self, :inspect, args, block)
  end
end
