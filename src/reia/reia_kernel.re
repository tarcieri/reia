module ReiaKernel    
  def puts(string)
    io::format("~s~n".to_list(), [string.to_list()])
    nil