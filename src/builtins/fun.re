#
# fun.re: Methods of the Fun builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Fun
  def call(self, :inspect, args, block)
    (:name,   name) = erl.fun_info(self, :name)
    (:module, mod)  = erl.fun_info(self, :module)
    "#<Fun #{name}:#{mod}>"
  end
  
  def call(self, :to_s, args, block)
    call(self, :inspect, args, block)
  end
end