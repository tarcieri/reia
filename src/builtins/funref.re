#
# funref.re: Methods of the Funref builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Funref
  def class; Funref; end
  
  def inspect
    (:reia_funref, receiver, name) = self
    "#<Funref #{receiver}:#{name}>"
  end
  
  def to_s
    inspect()
  end
end
