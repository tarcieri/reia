#
# Binary: Methods for the Binary pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Binary
  def funcall(binary, ~to_list, [])
    erlang::binary_to_list(binary)
    
  def funcall(binary, ~to_string, [])
    binary.to_list().to_string()
    
  def funcall(binary, ~to_s, [])
    funcall(binary, ~to_string, [])
    
  def funcall(binary, ~inspect, [])
    list = binary.to_list()
    if io_lib::char_list(list)
      ['<<"', list.to_string(), '">>'].join()
    else
      ["<<", list.join(","), ">>"].join()
      
