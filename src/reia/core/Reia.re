#
# Reia: The Reia interpreter
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Reia
  def start
    read_input([])
    
  def start(args)
    (~ok, input) = file::read_file(args[0])
    eval_input(input.to_string())

  def read_input(data)
    case io::get_line(~'')
      ~eof:
        input = data.map { |s| s.to_string() }.join()
        eval_input(input)
      string:
        read_input(data.push(string))
  
  def eval_input(input)
    try
      result = reia_eval::string(input)
      io::format("result: ~p~n".to_list(), [result])
      erlang::halt(0)
    catch ex
      io::format("exception: ~p~n".to_list(), [ex])
      io::format("stacktrace: ~p~n".to_list(), [erlang::get_stacktrace()])