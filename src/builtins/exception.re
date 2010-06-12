#
# exception.re: Common ancestor of all exceptions
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Exception
  def class; Exception; end
  
  def initialize(message)
    @message = message
  end
    
  def message
    @message
  end
end