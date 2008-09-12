%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, {normal, dict:new()}, fun reia_ssa:transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.
  
% Function declarations create a new scope
transform(State, {function, Line, Name, Arguments, Expressions}) ->
  % Create a new scope with dict:new()
  {ok, {_, Dict}, Arguments2} = reia_visitor:transform(Arguments, {argument, dict:new()}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict}, fun transform/2),
    
  % Return to the original scope
  {stop, State, {function, Line, Name, Arguments2, Expressions2}};
  
% Function names are identifiers and should remain undisturbed
transform({Mode, _Dict} = State, {funcall, Line, Name, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Node = {funcall, Line, Name, Arguments2},
  {stop, {Mode, Dict2}, Node};

transform({Mode, _Dict} = State, {funcall, Line, Receiver, Name, Arguments}) ->
  {ok, {_, Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, {_, Dict3}, Arguments2} = reia_visitor:transform(Arguments, {Mode, Dict2}, fun transform/2),
  Node = {funcall, Line, Receiver2, Name, Arguments2},
  {stop, {Mode, Dict3}, Node};

transform({Mode, _Dict} = State, {erl_funcall, Line, Module, Function, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Node = {erl_funcall, Line, Module, Function, Arguments2},
  {stop, {Mode, Dict2}, Node};
  
% Match expressions mutate variables on the LHS
transform({Mode, Dict}, {match, Line, In1, In2}) ->
  {ok, {_, Dict2}, Out2} = reia_visitor:transform(In2, {Mode, Dict}, fun transform/2),
  {ok, {_, Dict3}, Out1} = reia_visitor:transform(In1, {match, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {match, Line, Out1, Out2}};
  
% Arguments should initialize new entries in the SSA dict
transform({argument, Dict}, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      
      throw({error, {Name, "already bound"}});
    error ->
      Dict2 = dict:store(Name, 0, Dict),
      Node = {identifier, Line, ssa_name(Name, 0)},
      {stop, {argument, Dict2}, Node}
  end;
  
% Normally identifiers are mapped to their latest version
transform({normal, Dict} = State, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, Version} ->
      Node = {identifier, Line, ssa_name(Name, Version)},
      {stop, State, Node};
    error ->
      throw({error, {Name, "not bound"}})
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