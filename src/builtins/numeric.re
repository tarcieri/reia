#
# numeric.re: Methods common to Integers and Floats
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Numeric
  # alias_method :inspect, :to_s
  def inspect
    to_s()
  end
  
  # alias_method :to_i, :to_integer
  def to_i; to_integer(); end
  
  # alias_method :to_f, :to_float
  def to_f; to_float(); end
end