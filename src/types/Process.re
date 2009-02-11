#
# Process: Lightweight shared-nothing processes (thunking to Erlang's proc_lib)
# Copyright (C)2008-09 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Process
  # Process#spawn
  #   Create a new process
  def spawn(lambda)
    proc_lib::spawn(lambda)
    
  # Process#spawn_link
  #   Create a new process
  def spawn_link(lambda)
    proc_lib::spawn(lambda)
    
  # Process#to_s
  #   Generate a string representation of a process
  def funcall(pid, ~to_s, [])
    funcall(pid, ~inspect, [])

  # Process#inspect
  #   Inspect a process
  def funcall(pid, ~inspect, [])
    erlang::pid_to_list(pid).to_string()