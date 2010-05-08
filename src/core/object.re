#
# object.re: Common ancestor of all classes
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Object
  def initialize
    nil
  end
    
  def class
    Object
  end
    
  def to_s
    inspect()
  end
    
  def inspect
    "#<#{class()}>"
  end
    
  def method_missing(method, args)
    throw (:error, (method, "undefined"))
  end
end