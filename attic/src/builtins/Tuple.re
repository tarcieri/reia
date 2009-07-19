#
# Tuple: Methods for the Tuple builtin
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Tuple
  # Tuple#to_list
  #   Convert a tuple to a list
  def funcall(tuple, :to_list, [])
    erlang::tuple_to_list(tuple)
  end
  
  # Tuple#[]
  #   Retrieve an element from a Tuple
  def funcall(tuple, :'[]', [index])
    if index < 0
      erlang::element(tuple.size() + index + 1, tuple)
    else
      erlang::element(index + 1, tuple)
    end
  end
  
  # Tuple#to_s
  #   Generate a string representation of a Tuple  
  def funcall(tuple, :to_s, [])
    funcall(tuple, :inspect, [])
  end
  
  # Tuple#inspect
  #   Inspect the contents of a Tuple
  def funcall(tuple, :inspect, [])
    elems = tuple.to_list().map { |e| e.inspect() }.join(",")
    # special-case trailing comma
    if tuple.size() == 1
      "(#{elems},)"
    else
      "(#{elems})"
    end
  end
    
  # Tuple#size
  #   Return the number of elements in a Tuple
  def funcall(tuple, :size, [])
    erlang::tuple_size(tuple)
  end
end