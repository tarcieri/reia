module System
  # Return a list of arguments passed in from the command line
  def args
    erl.init.get_plain_arguments().map { |arg| arg.to_string() }
  end
    
  # Return the number of CPUs in the current system
  def cpus
    erl.system_info(:logical_processors)
  end
    
  # Halt the system, possibly returning the given status code
  def halt
    erl.halt()
  end
  def halt(status)
    erl.halt(status)
  end
    
  # Reboot the entire system, restarting the VM
  def reboot
    erl.init.reboot()
    nil
  end
    
  # Restart the system, preserving the same VM instance
  def restart
    erl.init.restart()
    nil
  end
end