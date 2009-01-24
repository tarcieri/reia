module System
  # Return a list of arguments passed in from the command line
  def args
    init::get_plain_arguments().map { |arg| arg.to_string() }
    
  # Return the number of CPUs in the current system
  def cpus
    erlang::system_info(~logical_processors)
    
  # Halt the system, possibly returning the given status code
  def halt
    erlang::halt()
  def halt(status)
    erlang::halt(status)
    
  # Reboot the entire system, restarting the VM
  def reboot
    init::reboot()
    nil
    
  # Restart the system, preserving the same VM instance
  def restart
    init::restart()
    nil