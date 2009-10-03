%
% reia_eval: Evaluate a given set of Reia expressions
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_eval).
-export([new_binding/0, string/1, string/2, exprs/2]).

% Create a new local variable binding
new_binding() -> [].

% Parse and evaluate the given string
string(Str) ->
	string(Str, []).

% Parse and evaluate the given string with the given binding
string(Str, Binding) ->
  case reia_parse:parse(Str) of
	  {fail, Error} ->
		  {error, Error};
		Expression ->
			exprs([Expression], Binding)
  end.

% Evaluate the given set of expressions
exprs(Expressions, Binding) ->
	io:format("Code: ~p~n", [Expressions]),
  {ok, Module} = reia_bytecode:compile(Expressions),
  {ok, Name, Value} = reia_bytecode:load(Module),

	% FIXME: In the future it's possible eval will create things which persist
	% beyond initial evaluation (e.g. lambdas, processes).  Once these features
	% are added a different solution will be needed than a simple code:purge.
  code:purge(Name),
  {value, Value, Binding}.