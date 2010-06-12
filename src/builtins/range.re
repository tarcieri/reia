#
# range.re: Methods of the Range builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Range
  def class; Range; end
  
  def to_list
    (:reia_range, from, to) = self
    erl.lists.seq(from, to)
  end
  
  def to_s
    (:reia_range, from, to) = self
    "#{from}..#{to}"
  end

  def inspect
    to_s()
  end
end
