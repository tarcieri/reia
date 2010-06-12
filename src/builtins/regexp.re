#
# regexp.re: Methods of the Regexp builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Regexp
  def class; Regexp; end
  
  def to_s
    (:reia_regexp, pattern) = self
    "%r/#{pattern}/"
  end
  
  def inspect
    to_s()
  end
end
