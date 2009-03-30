#
# Process: Methods for Process builtin: lightweight shared-nothing processes
# Copyright (C)2008-09 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Process
  # Process#spawn
  #   Create a new process
  def spawn(&block)
    proc_lib::spawn(block)
  end
    
  # Process#spawn_link
  #   Create a new process and links it to the current one
  def spawn_link(&block)
    proc_lib::spawn_link(block)
  end
    
  # Process#to_s
  #   Generate a string representation of a process
  def funcall(pid, :to_s, [])
    funcall(pid, :inspect, [])
  end

  # Process#inspect
  #   Inspect a process
  def funcall(pid, :inspect, [])
    erlang::pid_to_list(pid).to_string()
  end
end