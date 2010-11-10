#
# pid.re: Methods of the Pid builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Pid
  def class; Pid; end
  
  def to_string
    "#<Pid:#{erl.pid_to_list(self).to_string().sub('<', '')}"
  end
  
  def to_s; to_string(); end
  def inspect; to_s(); end
end
