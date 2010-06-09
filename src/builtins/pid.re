#
# pid.re: Methods of the Pid builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Pid
  def call(fake_self, :to_s, args, block)
    "#<Pid:#{erl.pid_to_list(fake_self).to_string().sub('<', '')}"
  end
  
  def call(fake_self, :inspect, args, block)
    fake_self.to_s()
  end
end
