# 
# Dir: Reia directory interface
# Copyright (C)2009-10 Tony Arcieri, Jared Kuolt
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module Dir
  def list(dir)
    case erl.file.list_dir(dir.to_list())
    when (:ok, filenames)
      [filename.to_string() for filename in filenames]
    when (:error, reason)
      throw(reason)
    end
  end
    
  def glob(path)
    [filename.to_string() for filename in erl.filelib.wildcard(path.to_list())]
  end
      
  def create(dir)
    case erl.file.make_dir(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw(reason)
    end
  end
      
  def delete(dir)
    case erl.file.del_dir(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw(reason)
    end
  end
      
  def cd(dir)
    case erl.file.set_cwd(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw(reason)
    end
  end
      
  def getcwd
    case erl.file.get_cwd()
    when (:ok, dir)
      dir.to_string()
    when (:error, reason)
      throw(reason)
    end
  end
  
  # Aliases
  def mkdir(dir); create(dir); end  
  def rmdir(dir); delete(dir); end
  def chdir(dir); cd(dir); end
  def pwd; getcwd(); end
end