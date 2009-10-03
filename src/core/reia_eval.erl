%
% reia_eval: Evaluate a given set of Reia expressions
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_eval).
-export([string/1, exprs/1]).

% Parse and evaluate the given string
string(Str) ->
  case reia_parse:parse(Str) of
	  {fail, Error} ->
		  {error, Error};
		Expression ->
			exprs([Expression])
  end.

exprs(Expressions) ->
  {ok, Module} = reia_bytecode:compile(Expressions),
  {ok, Name, Value} = reia_bytecode:load(Module),
  code:purge(Name),
  {value, Value}.