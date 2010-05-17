#
# pid.re: Methods of the Pid builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Pid
  def call(self, :to_s, args, block)
    "#<Pid:#{erl.pid_to_list(self).to_string().sub('<', '')}"
  end
  
  def call(self, :inspect, args, block)
    self.to_s()
  end
end