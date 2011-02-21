%
% reia_eval: Evaluate a given set of Reia expressions
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_eval).
-export([new_binding/0, string/1, string/2, exprs/1, exprs/2]).
-include("../compiler/reia_nodes.hrl").
-include("../compiler/reia_bindings.hrl").
-define(return_value_var(Line), #var{line=Line, name='__reia_eval_return_value'}).

% Create a new set of local variable bindings
new_binding() -> [].

% Parse and evaluate the given string
string(Str) -> string(Str, new_binding()).

% Parse and evaluate the given string with the given bindings
string(Str, Bindings) ->
  case reia_parse:string(Str) of
	  {error, _} = Error ->
	    Error;
		{ok, Exprs} ->
			exprs(Exprs, Bindings)
  end.

% Evaluate the given set of expressions
exprs(Exprs) -> exprs(Exprs, new_binding()).

% Evaluate the given set of expressions with the given bindings
exprs(Exprs, Bindings) ->
	% io:format("Input Code: ~p~n", [Exprs]),
	Exprs2 = annotate_return_value(Exprs, Bindings),
  Filename = "reia_eval#" ++ stamp(),
  Name = list_to_atom(Filename),

  {ok, Module} = reia_compiler:compile(
    Filename,
    [temporary_module(Name, [{var, 1, Var} || {Var, _} <- Bindings], Exprs2)],
    [{toplevel_wrapper, false}]
  ),

  Args = list_to_tuple([Val || {_, Val} <- Bindings]),
  {ok, Name, {Value, NewBindings}} = reia_bytecode:load(Module, [Args, nil]),

	% FIXME: This code:purge is just failing and modules are just accumulating
	% the code server whenever eval is used.  A "reaper" process is needed to
	% periodically try to purge these modules until it succeeds.
  code:purge(Name),
  
  {value, Value, NewBindings}.

% Generate a unique module name. Base it off the current PID
stamp() ->
  SplitPid = re:split(pid_to_list(self()), "\\."),
  string:join([binary_to_list(Num) || Num <- SplitPid], "_").

temporary_module(Name, Args, Exprs) ->
  #module{line=1, name=Name, exprs=[
    #function{line=1, name=toplevel, args=Args, body=Exprs}
  ]}.

% Annotate the return value of the expression to include the bindings
annotate_return_value(Exprs, Bindings) ->
  Exprs2 = case Exprs of
    []    -> [#nil{line=1}];
    [_|_] -> Exprs
  end,
  [LastExpr|Rest] = lists:reverse(Exprs2),
  Line = element(2, LastExpr),
  LastExpr2 = #match{line=Line, left=?return_value_var(Line), right=LastExpr},
  ReturnValue = return_value(output_bindings(Exprs2, Bindings), Line),
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
    #var{line=Line, name=Name}
  ]}.