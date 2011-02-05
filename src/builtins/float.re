#
# float.re: Methods of the Float builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Float < Numeric
  def class; Float; end
  
  def to_s
    erl.io_lib.format("~f".to_list(), [self]).to_string()
  end
  
  def to_integer; erl.round(self); end
  
  def to_float; self; end
end