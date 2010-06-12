#
# object.re: Common ancestor of all classes
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Object
  def class; Object; end
  
  def initialize
  end
    
  def class
    (:reia_module, erl.element(2, self))
  end
    
  def to_s
    inspect()
  end
    
  def inspect
    # Super secret ninja extract ivars from self
    ivars = erl.element(3, self)
    
    ivar_str = ivars.to_list().map do |(var, val)| 
      "@#{var}=#{val.inspect()}"
    end.join(" ")
    
    "#<#{class()} #{ivar_str}>"
  end
    
  def method_missing(method, args)
    # FIXME: throw unimplemented :(
    #throw (:error, (method, "undefined"))
  end
end