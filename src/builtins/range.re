#
# range.re: Methods of the Range builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Range
  def call(self, :to_list, args, block)
    (:reia_range, from, to) = self
    erl.lists.seq(from, to)
  end
  
  def call(self, :to_s, args, block)
    (:reia_range, from, to) = self
    "#{from}..#{to}"
  end

  def call(self, :inspect, args, block)
    self.to_s()
  end
end