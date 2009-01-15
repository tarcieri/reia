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
      (~ok, data):
        data.to_string()
      (~error, reason):
        throw reason

  def write(fn, data)
    case file::write_file(fn.to_list(), data.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def delete(fn)
    case file::delete(fn.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def copy(src, dest)
    case file::copy(src.to_list(), dest.to_list())
      (~ok, _):
        ~ok
      (~error, reason):
        throw reason

  def rename(src, dest)
    case file::rename(src.to_list(), dest.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason
  
  def list_dir(dir)
    case file::list_dir(dir.to_list())
      (~ok, filenames):
        filenames.map{|m| reia_erl::e2r(m).to_string()}
      (~error, reason):
        throw reason

  def make_dir(dir)
    case file::make_dir(dir.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def del_dir(dir)
    case file::del_dir(dir.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def set_cwd(dir)
    case file::set_cwd(dir.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def get_cwd
    case file::get_cwd()
      (~ok, dir):
        dir
      (~error, reason):
        throw reason

class IODevice
  # Mimicking Erlang's io_device returned from file:open
  def initialize(fn, modes)
    @modes = reia_erl::r2e(modes.map{|m| reia_erl::r2e(m)})
    @fn = fn.to_list()
    case file::open(@fn, @modes)
      (~ok, file):
        @file = file 
      (~error, reason):
        throw reason

  def read(num)
    case file::read(@file, reia_erl::r2e(num))
      (~ok, data):
        data.to_string()
      ~eof:
        ~eof
      (~error, reason):
        throw reason

  def close
    case file::close(@file)
      ~ok:
        ~ok
      (~error, reason):
        throw reason

  def write(data)
    case file::write(@file, data.to_list())
      ~ok:
        ~ok
      (~error, reason):
        throw reason
