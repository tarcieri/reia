#
# Main: Functions available in any scope
# The future of this module is uncertain!  These functions are presently not
# available in ANY scope except code evaluated by the Eval module.  Unless
# a better solution is found you may find these functions factored elsewhere.
#
# Copyright (C)2008 Tony Arcieri
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module Main
  # Print a string (with newline) to standard output
  def puts(string)
    io::format("~s~n".to_list(), [string.to_list()])
    nil
  end
    
  # Print a string (without newline) to standard output
  def print(string)
    io::format(string.to_list())
    nil
  end
  
  def eval(string)
    Eval.string(string)
  end
    
  def load(path)
    case file::read_file(path.to_list())
    when (~ok, data)
      Eval.string(data.to_string())
    when error
      error
    end
  end
end