#
# Lambda: Methods for the Lambda pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Lambda
  def funcall(lambda, :to_s, [])
    funcall(lambda, :inspect, [])
  end
    
  def funcall(lambda, :inspect, [])
    (:name, name)   = erlang::fun_info(lambda, :name)
    (:module, mod)  = erlang::fun_info(lambda, :module)
    "#<Lambda #{mod.to_s()}:#{name.to_s()}>"
  end
end