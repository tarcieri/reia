%
% String: Methods of the String builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('String').
-export([call/4]).
-include("../core/reia_types.hrl").

call(#reia_string{elements=Elements}, to_binary, _Args, _Block) ->
  iolist_to_binary(Elements);
  
call(String, to_list, _Args, _Block) ->
  binary_to_list(call(String, to_binary, {}, nil));
  
call(String, to_s, _Args, _Block) ->
  String;

call(String, print, Args, Block) ->
  io:format(call(String, to_list, Args, Block));
  
call(String, length, _Args, _Block) ->
  length(binary_to_list(call(String, to_binary, {}, nil)));

call(String, capitalize, Args, Block) ->
  [FirstLetter|Rest] = call(String, to_list, Args, Block),
  NewList = string:to_upper([FirstLetter]) ++ Rest,
  'List':call({NewList, to_string, [], nil}, nil);
  
call(String, to_module, Args, Block) ->
  List = call(String, to_list, Args, Block),
  #reia_module{name=list_to_atom(List)};
  
call(#reia_string{elements=Elements}, inspect, _Args, _Block) ->
  #reia_string{elements=[$", Elements, $"]}.