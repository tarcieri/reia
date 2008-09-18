#
# Lambda: Methods for the Lambda pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Lambda
  def funcall(lambda, ~to_s, [])
    funcall(lambda, ~inspect, [])
    
  def funcall(lambda, ~inspect, [])
    info = erlang::fun_info(lambda)
    (~value, (~name, name))   = lists::keysearch(~name,  1, info)
    (~value, (~module, mod))  = lists::keysearch(~module,  1, info)
    (~value, (~arity, arity)) = lists::keysearch(~arity, 1, info)
    ["#<Lambda ", name.to_s(), " (module: ", mod.to_s(), ", arity: ", arity.to_s(), ")>"].join()