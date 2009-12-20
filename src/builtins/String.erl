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
  
call(#reia_string{elements=Elements}, inspect, _Args, _Block) ->
  #reia_string{elements=[$", Elements, $"]}.