#
# range.re: Methods of the Range builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Range
  def call(fake_self, :to_list, args, block)
    (:reia_range, from, to) = fake_self
    erl.lists.seq(from, to)
  end
  
  def call(fake_self, :to_s, args, block)
    (:reia_range, from, to) = fake_self
    "#{from}..#{to}"
  end

  def call(fake_self, :inspect, args, block)
    fake_self.to_s()
  end
end
