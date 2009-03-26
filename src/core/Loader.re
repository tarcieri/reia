#
# Loader: Loads the Reia interpreter
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Loader
  def start
    read_input([])
  end
    
  def start(file)
    (:ok, input) = file::read_file(file)
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
end