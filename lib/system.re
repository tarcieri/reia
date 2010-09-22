module System
  # Display a string to the user (with appended newline)
  def puts(value)
    erl.io.format("~s~n".to_list(), [value.to_s().to_binary()])
    nil
  end
  
  # Display a string to the user (without any additional characters)
  def print(value)
    erl.io.format("~s".to_list(), [value.to_s().to_binary()])
    nil
  end
  
  # Load a Reia source code file
  def load(filename)
    case erl.reia.load(filename.to_s().to_list())
    when (:ok, _, _)
      true
    when (:error, :enoent)
      throw(FileNotFound, "No such file or directory - #{filename}")
    when (:error, error)
      throw(LoadError, error)
    end
  end
  
  # Argument list as passed in from the command line
  def args
    erl.init.get_plain_arguments().map { |arg| arg.to_string() }
  end
  
  # Environment variables, as a dict
  def env
    erl.os.getenv().map do |str| 
      [var, *rest] = str.to_string().split("=")
      (var, rest.join("="))
    end.to_dict()
  end
    
  # Number of CPUs in the current system
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