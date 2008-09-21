%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, ast/2, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ast(Ast, []).
  
ast(Ast, Variables) ->
  Dict = dict:from_list([{Variable, 0} || Variable <- Variables]),
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, {normal, Dict}, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.

% Module declarations create a new scope
transform(State, {module, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, {normal, dict:new()}, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {module, Line, Name, Expressions2}};
  
% Function declarations create a new scope
transform(State, {function, Line, Name, Arguments, Expressions}) ->
  % Create a new scope with dict:new()
  {ok, {_, Dict}, Arguments2} = reia_visitor:transform(Arguments, {argument, dict:new()}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict}, fun transform/2),
    
  % Return to the original scope
  {stop, State, {function, Line, Name, Arguments2, Expressions2}};

% Lambdas close over the outer scope, bind arguments, but don't affect the outer scope
transform({_, Dict} = State, {lambda, Line, Arguments, Expressions}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, {argument, Dict}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict2}, fun transform/2),
  {stop, State, {lambda, Line, Arguments2, Expressions2}};
  
% Function names are identifiers and should remain undisturbed unless they are bound variables
transform({Mode, Dict} = State, {funcall, Line, {identifier, _Line, Name} = Identifier, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Identifier2 = case dict:find(Name, Dict) of
    {ok, Version} ->
      {variable, Line, ssa_name(Name, Version)};
    error ->
      Identifier
  end,  
  Node = {funcall, Line, Identifier2, Arguments2},
  {stop, {Mode, Dict2}, Node};

transform({Mode, _Dict} = State, {funcall, Line, Receiver, Name, Arguments}) ->
  {ok, {_, Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, {_, Dict3}, Arguments2} = reia_visitor:transform(Arguments, {Mode, Dict2}, fun transform/2),
  Node = {funcall, Line, Receiver2, Name, Arguments2},
  {stop, {Mode, Dict3}, Node};

transform({Mode, _Dict} = State, {funcall, Line, Receiver, Name, Arguments, Block}) ->
  {ok, {_, Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, {_, Dict3}, Arguments2} = reia_visitor:transform(Arguments, {Mode, Dict2}, fun transform/2),
  {ok, {_, Dict4}, Block2} = reia_visitor:transform(Block, {Mode, Dict3}, fun transform/2),
  Node = {funcall, Line, Receiver2, Name, Arguments2, Block2},
  {stop, {Mode, Dict4}, Node};

transform({Mode, _Dict} = State, {erl_funcall, Line, Module, Function, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Node = {erl_funcall, Line, Module, Function, Arguments2},
  {stop, {Mode, Dict2}, Node};
  
% Arguments should initialize new entries in the SSA dict
transform({argument, Dict}, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      throw({error, {Line, lists:flatten(io_lib:format("argument already bound: '~s'", [Name]))}});
    error ->
      Dict2 = dict:store(Name, 0, Dict),
      Node = {identifier, Line, ssa_name(Name, 0)},
      {stop, {argument, Dict2}, Node}
  end;

% Match expressions mutate variables on the LHS
transform({Mode, Dict}, {match, Line, In1, In2}) ->
  {ok, {_, Dict2}, Out2} = reia_visitor:transform(In2, {Mode, Dict}, fun transform/2),
  {ok, {_, Dict3}, Out1} = reia_visitor:transform(In1, {match, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {match, Line, Out1, Out2}};

% Case expressions bind variables in clauses
transform({Mode, Dict}, {'case', Line, Expression, Clauses}) ->
  {ok, {_, Dict2}, Expression2} = reia_visitor:transform(Expression, {Mode, Dict}, fun transform/2),
  {ok, {_, Dict3}, Clauses2} = reia_visitor:transform(Clauses, {'case', Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {'case', Line, Expression2, Clauses2}};

% Case clauses match against patterns
transform({'case', Dict}, {clause, Line, Pattern, Expressions}) ->
  {ok, {_, Dict2}, Pattern2} = reia_visitor:transform(Pattern, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict2}, fun transform/2),
  {stop, {'case', Dict3}, {clause, Line, Pattern2, Expressions2}};
    
% List comprehensions can access the outer scope but have a scope of their own
transform({_, Dict} = State, {'lc', Line, Transform, Expressions}) ->
  {ok, {_, Dict2}, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict}, fun transform/2),
  {ok, _, Transform2} = reia_visitor:transform(Transform, {normal, Dict2}, fun transform/2),
  {stop, State, {'lc', Line, Transform2, Expressions2}};

% Generate expressions match a pattern
transform({Mode, Dict}, {'generate', Line, Pattern, List}) ->
  {ok, {_, Dict2}, Pattern2} = reia_visitor:transform(Pattern, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, List2} = reia_visitor:transform(List, {Mode, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {'generate', Line, Pattern2, List2}};

% Normally identifiers are mapped to their latest version
transform({normal, Dict} = State, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, Version} ->
      Node = {identifier, Line, ssa_name(Name, Version)},
      {stop, State, Node};
    error ->
      throw({error, {Line, lists:flatten(io_lib:format("unbound variable: '~s'", [Name]))}})
  end;
  
% On the LHS of match expressions, variables are assigned a new version
transform({match, Dict}, {identifier, Line, Name}) ->
  {Dict2, Version2} = case dict:find(Name, Dict) of
    {ok, Version} ->
      {dict:store(Name, Version + 1, Dict), Version + 1};
    error ->
      {dict:store(Name, 0, Dict), 0}
  end,
  Node = {identifier, Line, ssa_name(Name, Version2)},
  {stop, {match, Dict2}, Node};
    
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.
  
% Generate the SSA name for a given variable, which takes the form name_version
ssa_name(Name, Version) ->
  Name2 = lists:flatten(io_lib:format("~s_~w", [Name, Version])),
  list_to_atom(Name2).
