# 
# File: Reia file interface
# Copyright (C)2008 Jared Kuolt
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module File
  def open(fn)
    IODevice(fn, [~read,~binary])

  def open(fn, modes)
    IODevice(fn, modes)

  def read(fn)
    case file::read_file(fn.to_list())
    when (~ok, data)
      data
    when (~error, reason)
      throw reason

  def write(fn, data)
    case file::write_file(fn.to_list(), data.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def rm(fn)
    delete(fn)
    
  def delete(fn)
    case file::delete(fn.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def cp(src, dest)
    copy(src, dest)
    
  def copy(src, dest)
    case file::copy(src.to_list(), dest.to_list())
    when (~ok, _)
      ~ok
    when (~error, reason)
      throw reason

  def mv(src, dest)
    move(src, dest)
    
  def move(src, dest)
    case file::rename(src.to_list(), dest.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason
  
  def size(path)
    filelib::file_size(path.to_list())
    
  def file?(path)
    filelib::is_regular(path.to_list())
    
  def dir?(path)
    directory?(path)
    
  def directory?(path)
    filelib::is_dir(path.to_list())
  
  def link?(path)
    symlink?(path)
    
  def symlink?(path)
    case file::read_link(path.to_list())
    when (~ok, _)
      true
    when _
      false

class IODevice
  # Mimicking Erlang's io_device returned from file:open
  def initialize(fn, modes)
    @modes = reia_erl::r2e(modes.map{|m| reia_erl::r2e(m)})
    @fn = fn.to_list()
    case file::open(@fn, @modes)
    when (~ok, file)
      @file = file 
    when (~error, reason)
      throw reason

  def read(num)
    case file::read(@file, reia_erl::r2e(num))
    when (~ok, data)
      data
    when ~eof
      ~eof
    when (~error, reason)
      throw reason

  def close
    case file::close(@file)
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def write(data)
    case file::write(@file, data.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason