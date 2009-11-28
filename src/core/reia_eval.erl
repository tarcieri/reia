%
% reia_eval: Evaluate a given set of Reia expressions
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_eval).
-export([new_binding/0, string/1, string/2, exprs/2]).
-include("../compiler/reia_bindings.hrl").

% Create a new local variable binding
new_binding() -> [].

% Parse and evaluate the given string
string(Str) ->
	string(Str, []).

% Parse and evaluate the given string with the given binding
string(Str, Bindings) ->
  case reia_parse:string(Str) of
	  {error, _} = Error ->
	    Error;
		{ok, Exprs} ->
			exprs(Exprs, Bindings)
  end.

% Evaluate the given set of expressions
exprs(Exprs, Bindings) ->
	io:format("Input Code: ~p~n", [Exprs]),
	NewBindings = output_bindings(Exprs),
	io:format("New Bindings: ~p~n", [NewBindings]),

  {ok, Module} = reia_compiler:compile(temporary_module(), Exprs),
  {ok, Name, Value} = reia_bytecode:load(Module),

	% FIXME: In the future it's possible eval will create things which persist
	% beyond initial evaluation (e.g. lambdas, processes).  Once these features
	% are added a different solution will be needed than a simple code:purge.
  code:purge(Name),
  {value, Value, Bindings}.

% Generate a temporary module name
temporary_module() ->
  Hash = erlang:md5(term_to_binary(make_ref())),
  Nonce = lists:flatten([io_lib:format("~.16b",[N]) || <<N>> <= Hash]),
  "reia_eval#" ++ Nonce.

% Obtain a list of all variables which will be bound when eval is complete
output_bindings(Exprs) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs),
  [#bindings{entries=Entries}|_] = lists:reverse(BAExprs),
  dict:fetch_keys(Entries).