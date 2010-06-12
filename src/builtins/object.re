#
# object.re: Common ancestor of all classes
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Object
  def initialize
  end
    
  def class
    (:reia_module, erl.element(2, self))
  end
    
  def to_s
    inspect()
  end
  
  def instance_variables
    # Super secret ninja extract ivars from self
    erl.element(3, self)
  end
    
  def inspect    
    if instance_variables().empty?()
      "#<#{class()}>"
    else    
      ivar_str = instance_variables().to_list().map do |(var, val)|
        "@#{var}=#{val.inspect()}"
      end.join(" ")
    
      "#<#{class()} #{ivar_str}>"
    end
  end
    
  def method_missing(method, args)
    # FIXME: throw unimplemented :(
    #throw (:error, (method, "undefined"))
    (:error, class(), :undefined_method, method, args)
  end
end