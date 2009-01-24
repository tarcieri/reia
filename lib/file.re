# 
# File: Reia file interface
# Copyright (C)2008 Jared Kuolt
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module File
  def open(fn)
    IODevice(fn, [~read])

  def open(fn, modes)
    IODevice(fn, modes)

  def read(fn)
    case file::read_file(fn.to_list())
    when (~ok, data)
      data.to_string()
    when (~error, reason)
      throw reason

  def write(fn, data)
    case file::write_file(fn.to_list(), data.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def delete(fn)
    case file::delete(fn.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def copy(src, dest)
    case file::copy(src.to_list(), dest.to_list())
    when (~ok, _)
      ~ok
    when (~error, reason)
      throw reason

  def rename(src, dest)
    case file::rename(src.to_list(), dest.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason
  
  def list_dir(dir)
    case file::list_dir(dir.to_list())
    when (~ok, filenames)
      filenames.map{|m| reia_erl::e2r(m).to_string()}
    when (~error, reason)
      throw reason

  def make_dir(dir)
    case file::make_dir(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def del_dir(dir)
    case file::del_dir(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def set_cwd(dir)
    case file::set_cwd(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def get_cwd
    case file::get_cwd()
    when (~ok, dir)
      dir
    when (~error, reason)
      throw reason

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
      data.to_string()
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