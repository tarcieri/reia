#
# Numeric: Methods for the Numeric pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Numeric
  def pow(base, exponent)
    result = math::pow(base, exponent)
    if erlang::is_integer(base)
      result.round()
    else
      result
    end
  end

  def funcall(number, :round, [])
    erlang::round(number)
  end

  def funcall(number, :abs, [])
    erlang::abs(number)
  end
  
  def funcall(number, :to_string, [])
    number.to_list().to_string()
  end
        
  def funcall(number, :to_s, [])
    funcall(number, :to_string, [])
  end
    
  def funcall(number, :inspect, [])
    funcall(number, :to_string, [])
  end

  def funcall(number, :to_list, [])
    if erlang::is_integer(number)
      erlang::integer_to_list(number)
    else
      [list] = io_lib::format("~f".to_list(), [number])
      list
    end
  end
end