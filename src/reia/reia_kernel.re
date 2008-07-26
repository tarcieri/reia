#
# reia_kernel: Methods available locally within any scope
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module ReiaKernel    
  def puts(string)
    io::format("~s~n".to_list(), [string.to_list()])
    nil