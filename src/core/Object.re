#
# Object: Common ancestor of all classes
# Copyright (C)2008-09 Tony Arcieri
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
    
  def pid
    erlang::self()
  end
    
  def to_s
    pid_str = pid().to_s().sub(/^</, '')
    "#<#{class()}:#{pid_str}"
  end
    
  def inspect
    to_s()
  end
    
  def _(method, args)
    throw (~error, (method, "undefined"))
  end
    
  def handle_message(message)
    nil
  end
end