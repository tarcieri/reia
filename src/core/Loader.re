#
# Loader: Loads the Reia interpreter
# Copyright (C)2008-09 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Loader
  def start
    stdlib()
    read_input([])
  end
  def start(filename)
    stdlib()
    file(filename)
  end
    
  def file(name)
    (:ok, input) = file::read_file(name.to_list())
    eval_input(input.to_string())
  end

  def read_input(data)
    case io::get_line(:'')
    when :eof
      input = data.map { |s| s.to_string() }.join()
      eval_input(input)
    when string
      read_input(data.push(string))
    end
  end
  
  def eval_input(input)
    try
      Eval.string(input)
      erlang::halt(0)
    catch ex
      io::format("exception: ~p~n".to_list(), [ex])
      io::format("stacktrace: ~p~n".to_list(), [erlang::get_stacktrace()])
    end
  end
  
  # Load the Reia standard library
  def stdlib
    # Resolve the location of the Reia path.  Try code:lib_dir() first
    reia_dir = "#{code::lib_dir().to_string()}/reia"
    if filelib::is_dir(reia_dir.to_list())
      __load_stdlib("#{reia_dir}/lib")
    else
      # If that fails, look in the current directory
      path = __getcwd()
      if /reia/.match(path)
        __load_stdlib(path.sub(/reia.*$/, "reia/lib"))
      else
        throw "can't load stdlib"
      end
    end
  end
  
  # Ordinarily this would use Dir.list but that's in the standard library,
  # and the standard library loader is what needs it  
  def __dirlist(dir)
    case file::list_dir(dir.to_list())
    when (:ok, filenames)
      [filename.to_string() | filename in filenames]
    when (:error, reason)
      throw reason
    end
  end
  
  # Ordinarily this would use Dir.getcwd but that's in the standard library,
  # and the standard library loader is what needs it
  def __getcwd
    case file::get_cwd()
    when (:ok, dir)
      dir.to_string()
    when (:error, reason)
      throw reason
    end
  end
  
  # Load all the files underneath the given stdlib path
  def __load_stdlib(stdlib_dir)
    Main.print("Loading standard library from #{stdlib_dir}... ")
    
    __dirlist(stdlib_dir).each do |file|
      Main.load("#{stdlib_dir}/#{file}")
    end
    
    Main.puts("done.")
  end
end