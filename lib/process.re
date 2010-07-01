#
# process.re: lightweight shared-nothing processes
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Process
  # Create a new process
  def spawn(&block)
    erl.proc_lib.spawn(block)
  end
    
  # Create a new process and links it to the current one
  def spawn_link(&block)
    erl.proc_lib.spawn_link(block)
  end
  
  # Retrieve the PID of the current process
  def pid; erl.self(); end
  
  # Link to another process
  def link(pid); erl.link(pid); end
end