#
# regexp.re: Methods of the Regexp builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Regexp
  def call(self, :to_s, args, block)
    (:reia_regexp, pattern) = self
    "%r/#{pattern}/"
  end
  
  def call(self, :inspect, args, block)
    self.to_s()
  end
end