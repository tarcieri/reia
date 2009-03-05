%
% Constant: Methods for the String pseudo-class
% Copyright (C)2008-09 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Constant').
-export([funcall/3]).

funcall({constant, Name}, inspect, []) ->
  case code:ensure_loaded(Name) of
    {module, _Name} ->
      reia_string:from_list(atom_to_list(Name));
    {error, _Error} ->
      throw({error, {Name, "not loaded"}})
  end;  
funcall(Constant, to_s, []) ->
  funcall(Constant, inspect, []);
funcall({constant, Name}, Method, Arguments) ->
  apply(Name, Method, Arguments).
