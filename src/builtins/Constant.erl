%
% Constant: Methods for the String pseudo-class
% Copyright (C)2008-09 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Constant').
-export([funcall/4]).

funcall({constant, Name}, inspect, [], _Block) ->
  case code:ensure_loaded(Name) of
    {module, _Name} ->
      reia_string:from_list(atom_to_list(Name));
    {error, _Error} ->
      throw({error, {Name, "not loaded"}})
  end;  
funcall(Constant, to_s, [], Block) ->
  funcall(Constant, inspect, [], Block);
funcall({constant, Name}, Method, Arguments, Block) ->
  Name:Method(list_to_tuple(Arguments), Block).