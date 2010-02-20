# 
# Dir: Reia directory interface
# Copyright (C)2009 Tony Arcieri, Jared Kuolt
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module Dir
  def list(dir)
    case file::list_dir(dir.to_list())
    when (:ok, filenames)
      [filename.to_string() | filename in filenames]
    when (:error, reason)
      throw reason
    end
  end
    
  def glob(path)
    [filename.to_string() | filename in filelib::wildcard(path.to_list())]
  end
  
  def mkdir(dir)
    create(dir)
  end
    
  def create(dir)
    case file::make_dir(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def rmdir(dir)
    delete(dir)
  end
    
  def delete(dir)
    case file::del_dir(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def chdir(dir)
    cd(dir)
  end
    
  def cd(dir)
    case file::set_cwd(dir.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def pwd
    getcwd()
  end
    
  def getcwd
    case file::get_cwd()
    when (:ok, dir)
      dir.to_string()
    when (:error, reason)
      throw reason
    end
  end
end