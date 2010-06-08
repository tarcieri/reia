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
    # FIXME: bleh, need to fix function calls within objects
    #inspect()
    nil
  end
    
  def inspect
    "#<#{class()}>"
  end
    
  def method_missing(method, args)
    # FIXME: throw unimplemented :(
    #throw (:error, (method, "undefined"))
    nil
  end
end