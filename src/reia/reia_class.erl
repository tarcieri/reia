%
% reia_module: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_class).
-export([build/1]).

build({class, _Line, Name, Functions}) ->
  io:format("class: ~p: ~n~p~n", [Name, Functions]);
build(_) ->
  {error, "invalid class"}.