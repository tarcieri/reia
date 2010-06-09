#
# regexp.re: Methods of the Regexp builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Regexp
  def call(fake_self, :to_s, args, block)
    (:reia_regexp, pattern) = fake_self
    "%r/#{pattern}/"
  end
  
  def call(fake_self, :inspect, args, block)
    fake_self.to_s()
  end
end
