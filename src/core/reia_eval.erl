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
-define(return_value_var, #identifier{line=1, name='__reia_eval_return_value'}).

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
exprs(Exprs, _Bindings) ->
	io:format("Input Code: ~p~n", [Exprs]),
	Exprs2 = annotate_return_value(Exprs),

  {ok, Module} = reia_compiler:compile(temporary_module(), Exprs2),
  {ok, Name, {Value, NewBindings}} = reia_bytecode:load(Module),

	% FIXME: In the future it's possible eval will create things which persist
	% beyond initial evaluation (e.g. lambdas, processes).  Once these features
	% are added a different solution will be needed than a simple code:purge.
  code:purge(Name),
  {value, Value, NewBindings}.

% Generate a temporary module name
temporary_module() ->
  Hash = erlang:md5(term_to_binary(make_ref())),
  Nonce = lists:flatten([io_lib:format("~.16b",[N]) || <<N>> <= Hash]),
  "reia_eval#" ++ Nonce.

% Annotate the return value of the expression to include the bindings
annotate_return_value(Exprs) ->
  [LastExpr|Rest] = lists:reverse(Exprs),
  Line = element(2, LastExpr),
  LastExpr2 = #match{line=Line, left=?return_value_var, right=LastExpr},
  ReturnValue = return_value(output_bindings(Exprs), Line),
  lists:reverse([ReturnValue, LastExpr2 | Rest]).

% Obtain a list of all variables which will be bound when eval is complete
output_bindings(Exprs) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs),
  [#bindings{entries=Entries}|_] = lists:reverse(BAExprs),
  dict:fetch_keys(Entries).

% Generate the return value for eval, appending the binding nodes
return_value(Bindings, Line) ->
  {tuple, Line, [
      ?return_value_var,
      binding_forms([binding_node(Name, Line) || Name <- Bindings], Line)
  ]}.

% Generate the AST representing a given binding
binding_node(Name, Line) ->
  {tuple, Line, [{atom, Line, Name}, {identifier, Line, Name}]}.

% Explode bindings into cons expressions
binding_forms([], Line) ->
  {empty, Line};
binding_forms([Expr|Rest], Line) ->
  {cons, Line, Expr, binding_forms(Rest, Line)}.