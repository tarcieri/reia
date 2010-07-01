#
# main.re: Methods available in all scopes
# Copyright (C)2009-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Main
  def puts(value)
    erl.io.format("~s~n".to_list(), [value.to_s().to_binary()])
    nil
  end

  def print(value)
    erl.io.format("~s".to_list(), [value.to_s().to_binary()])
    nil
  end
  
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
end