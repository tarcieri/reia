#
# Fun: Methods for the Fun builtin
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Fun
  def funcall(fn, :to_s, [])
    funcall(fn, :inspect, [])
  end
    
  def funcall(fn, :inspect, [])
    (:name, name)   = erlang::fun_info(fn, :name)
    (:module, mod)  = erlang::fun_info(fn, :module)
    "#<Fun #{mod.to_s()}:#{name.to_s()}>"
  end
end