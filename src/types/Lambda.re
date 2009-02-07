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
    (~name, name)   = erlang::fun_info(lambda, ~name)
    (~module, mod)  = erlang::fun_info(lambda, ~module)
    (~arity, arity) = erlang::fun_info(lambda, ~arity)
    ["#<Lambda ", name.to_s(), " (module: ", mod.to_s(), ", arity: ", arity.to_s(), ")>"].join()