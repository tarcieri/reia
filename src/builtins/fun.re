#
# fun.re: Methods of the Fun builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Fun
  def to_s
    (:name,   name) = erl.fun_info(self, :name)
    (:module, mod)  = erl.fun_info(self, :module)
    "#<Fun #{name}:#{mod}>"
  end
  
  def inspect
    to_s()
  end
end
