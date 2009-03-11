# 
# File: Reia file interface
# Copyright (C)2008 Jared Kuolt, Tony Arcieri
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module File
  def open(fn)
    IODevice(fn, [:read, :binary])
  end
  
  def open(fn, modes)
    IODevice(fn, modes)
  end
  
  def read(fn)
    case file::read_file(fn.to_list())
    when (:ok, data)
      data
    when (:error, reason)
      throw reason
    end
  end
  
  def write(fn, data)
    case file::write_file(fn.to_list(), data.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end

  def rm(fn)
    delete(fn)
  end
    
  def delete(fn)
    case file::delete(fn.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def cp(src, dest)
    copy(src, dest)
  end
    
  def copy(src, dest)
    case file::copy(src.to_list(), dest.to_list())
    when (:ok, _)
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def mv(src, dest)
    move(src, dest)
  end
    
  def move(src, dest)
    case file::rename(src.to_list(), dest.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def size(path)
    filelib::file_size(path.to_list())
  end
    
  def file?(path)
    filelib::is_regular(path.to_list())
  end
    
  def dir?(path)
    directory?(path)
  end
    
  def directory?(path)
    filelib::is_dir(path.to_list())
  end
  
  def link?(path)
    symlink?(path)
  end
    
  def symlink?(path)
    case file::read_link(path.to_list())
    when (:ok, _)
      true
    when _
      false
    end
  end
end

class IODevice
  # Mimicking Erlang's io_device returned from file:open
  def initialize(fn, modes)
    @modes = reia_erl::r2e(modes.map{|m| reia_erl::r2e(m)})
    @fn = fn.to_list()
    case file::open(@fn, @modes)
    when (:ok, file)
      @file = file 
    when (:error, reason)
      throw reason
    end
  end

  def read(num)
    case file::read(@file, reia_erl::r2e(num))
    when (:ok, data)
      data
    when :eof
      :eof
    when (:error, reason)
      throw reason
    end
  end
  
  def close
    case file::close(@file)
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
  
  def write(data)
    case file::write(@file, data.to_list())
    when :ok
      :ok
    when (:error, reason)
      throw reason
    end
  end
end