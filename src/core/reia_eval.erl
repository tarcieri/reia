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
  case reia_parse:string(Str) of
	  {error, _} = Error ->
	    Error;
		{ok, Exprs} ->
			exprs(Exprs, Binding)
  end.

% Evaluate the given set of expressions
exprs(Expressions, Binding) ->
	io:format("Input Code: ~p~n", [Expressions]),
  {ok, Module} = reia_compiler:compile(nonce_filename(), Expressions),
  {ok, Name, Value} = reia_bytecode:load(Module),

	% FIXME: In the future it's possible eval will create things which persist
	% beyond initial evaluation (e.g. lambdas, processes).  Once these features
	% are added a different solution will be needed than a simple code:purge.
  code:purge(Name),
  {value, Value, Binding}.

nonce_filename() ->
  ["#Ref" ++ Ref] = io_lib:format("~p", [make_ref()]),
  "reia_eval#" ++ Ref.