%
% reia_eval: Evaluate a given set of Reia expressions
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_eval).
-export([new_binding/0, string/1, string/2, exprs/2]).
-include("../compiler/reia_nodes.hrl").
-include("../compiler/reia_bindings.hrl").
-define(return_value_var(Line), #identifier{line=Line, name='__reia_eval_return_value'}).

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
	Exprs2 = annotate_return_value(Exprs, Bindings),

  {ok, Module} = reia_compiler:compile(
    temporary_module(),
    Exprs2,
    [{toplevel_args, [Var || {Var, _} <- Bindings]}]
  ),

  Args = [Val || {_, Val} <- Bindings],
  {ok, Name, {Value, NewBindings}} = reia_bytecode:load(Module, Args),

	% FIXME: In the future it's possible eval will create things which persist
	% beyond initial evaluation (e.g. lambdas, processes).  Once these features
	% are added a different solution will be needed than a simple code:purge.
  code:purge(Name),
  {value, Value, NewBindings}.

% Generate a temporary module name
temporary_module() ->
  RawHash = erlang:md5(term_to_binary(make_ref())),
  HexHash = lists:flatten([io_lib:format("~.16b",[N]) || <<N>> <= RawHash]),
  "reia_eval#" ++ HexHash.

% Annotate the return value of the expression to include the bindings
annotate_return_value(Exprs, Bindings) ->
  [LastExpr|Rest] = lists:reverse(Exprs),
  Line = element(2, LastExpr),
  LastExpr2 = #match{line=Line, left=?return_value_var(Line), right=LastExpr},
  ReturnValue = return_value(output_bindings(Exprs, Bindings), Line),
  lists:reverse([ReturnValue, LastExpr2 | Rest]).

% Obtain a list of all variables which will be bound when eval is complete
output_bindings(Exprs, Bindings) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs),
  [#bindings{entries=NewBindings}|_] = lists:reverse(BAExprs),
  lists:usort([Var || {Var, _} <- Bindings] ++ dict:fetch_keys(NewBindings)).

% Generate the return value for eval, appending the binding nodes
return_value(Bindings, Line) ->
  #tuple{line=Line, elements = [?return_value_var(Line), bindings_list(Bindings, Line)]}.

% Construct the output list for the bindings
bindings_list([], Line) ->
  {empty, Line};
bindings_list([Name|Rest], Line) ->
  {cons, Line, binding_node(Name, Line), bindings_list(Rest, Line)}.

% Generate the AST representing a given binding
binding_node(Name, Line) ->
  #tuple{line=Line, elements=[
    #atom{line=Line, name=Name},
    #identifier{line=Line, name=Name}
  ]}.