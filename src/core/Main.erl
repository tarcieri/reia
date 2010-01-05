%
% Main: Methods of the Main builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Main').
-export([puts/2, print/2, load/2]).
-include("reia_types.hrl").

puts({Value}, _Block) ->
  io:format("~s~n", [rvalue_to_list(Value)]).

print({Value}, _Block) ->
  io:format(rvalue_to_list(Value)).

load({Filename}, _Block) ->
  reia:load(rvalue_to_list(Filename)).
  
rvalue_to_list(Value) ->
  #reia_string{elements=Elements} = reia_dispatch:call(Value, to_s, {}, nil),
  binary_to_list(iolist_to_binary(Elements)).