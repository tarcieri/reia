# 
# Dir: Reia directory interface
# Copyright (C)2009 Tony Arcieri
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
module Dir
  def list(dir)
    case file::list_dir(dir.to_list())
    when (~ok, filenames)
      [filename.to_string() | filename in filenames]
    when (~error, reason)
      throw reason
      
  def glob(path)
    [filename.to_string() | filename in filelib::wildcard(path.to_list())]

  def mkdir(dir)
    create(dir)
    
  def create(dir)
    case file::make_dir(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def rmdir(dir)
    delete(dir)
    
  def delete(dir)
    case file::del_dir(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def chdir(dir)
    cd(dir)
    
  def cd(dir)
    case file::set_cwd(dir.to_list())
    when ~ok
      ~ok
    when (~error, reason)
      throw reason

  def pwd
    getcwd()
    
  def getcwd
    case file::get_cwd()
    when (~ok, dir)
      dir.to_string()
    when (~error, reason)
      throw reason