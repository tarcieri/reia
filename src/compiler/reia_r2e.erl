%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_types.hrl").
-define(reia_dispatch(Receiver, Line, Method, Args, Block),
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, call}},
    [
      transform(Receiver),
      {atom, Line, Method},
      {tuple, Line, [transform(Arg) || Arg <- Args]},
      transform(Block)
    ]
  }
).

% Lists of expressions
transform(Exprs, _Options) ->
  [transform(Expr) || Expr <- Exprs].

% Modules
transform(#module{line=Line, name=Name, functions=Funcs}) ->
  % Modules need some postprocessing, so we leave them in a similar form but
  % with their subtrees converted
  #module{
    line = Line,
    name = Name,
    functions = group_clauses([transform(Func) || Func <- Funcs])
  };

% Functions
transform(#function{line=Line, name=Name, arguments=Args, block=Block, body=Exprs}) ->
  {function, Line, Name, 2, [{clause, Line,
    [{tuple, Line, [transform(Arg) || Arg <- Args]}, transform(Block)],
    [],
    [transform(Expr) || Expr <- Exprs]
  }]};

% Terminals
transform(#integer{} = Expr) -> Expr;
transform(#float{} = Expr)   -> Expr;
transform(#atom{} = Expr)    -> Expr;
transform(#true{line=Line})  -> {atom, Line, true};
transform(#false{line=Line}) -> {atom, Line, false};
transform(#nil{line=Line})   -> {atom, Line, nil};
transform(#identifier{line=Line, name=Name}) -> {var, Line, Name};
transform(#string{line=Line, characters=Chars}) ->
  {tuple, Line, [
    {atom, Line, reia_string},
    {cons, Line,
      {bin, Line, [{bin_element, Line, {string, Line, Chars}, default, default}]},
      {nil, Line}
    }
  ]};

% Matches
transform(#match{line=Line, left=Left, right=Right}) ->
  {match, Line, transform(Left), transform(Right)};

% Lists
transform(#cons{line=Line, expr=Expr, tail=Tail}) ->
  {cons, Line, transform(Expr), transform(Tail)};

transform(#empty{line=Line}) ->
  {nil, Line};

% Tuples
transform(#tuple{line=Line, elements=Exprs}) ->
  {tuple, Line, [transform(Expr) || Expr <- Exprs]};

% Maps
transform(#dict{line=Line, elements=Elements}) ->
  {call, Line,
    {remote, Line, {atom, Line, dict}, {atom, Line, from_list}},
    [dict_elements(Elements, Line)]
  };

% Operators
transform(#unary_op{line=Line, type=Type, val=Val}) ->
  {op, Line, Type, transform(Val)};

transform(#binary_op{line=Line, type='**', left=Left, right=Right}) ->
  {call, Line,
    {remote, Line, {atom, Line, math}, {atom, Line, pow}},
    [transform(Left), transform(Right)]
  };

transform(#binary_op{line=Line, type='[]', left=Left, right=Right}) ->
  ?reia_dispatch(Left, Line, '[]', [Right], transform(#nil{line=Line}));

transform(#binary_op{line=Line, type=Type, left=Left, right=Right}) ->
  {op, Line, Type, transform(Left), transform(Right)};

% Function calls
transform(#remote_call{
  line      = Line,
  receiver  = Receiver,
  name      = Name,
  arguments = Args,
  block     = Block
}) -> ?reia_dispatch(Receiver, Line, Name, Args, Block);

transform(#native_call{
  line      = Line,
  module    = Module,
  function  = Function,
  arguments = Args
}) ->
  {call, Line,
    {remote, Line, {atom, Line, Module}, {atom, Line, Function}},
    [transform(Arg) || Arg <- Args]
  };

% Code blocks (Erlang-style, not to be confused with Ruby-style block arguments)
transform(#block{line=Line, exprs=Exprs}) -> 
  {block, Line, [transform(Expr) || Expr <- Exprs]}.

%% Group clauses of functions with the same name and arity
group_clauses(Functions) ->
  group_clauses(Functions, dict:new()).
group_clauses([], Functions) ->
  [
    {function, Line, Name, Arity, lists:reverse(Clauses)} ||
    {{Name, Arity},{Line, Clauses}} <- dict:to_list(Functions)
  ];
group_clauses([Function|Rest], Functions) ->
  {function, Line, Name, Arity, [Clause]} = Function,
  case dict:find({Name, Arity}, Functions) of
    {ok, {Line2, Clauses}} ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line2, [Clause|Clauses]}, Functions));
    error ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line, [Clause]}, Functions))
  end.

%% Transform the elements of Dicts
dict_elements([], Line) ->
  {nil, Line};
dict_elements([{Key,Value}|Rest], Line) ->
  {cons, Line, {tuple, Line, [transform(Key), transform(Value)]}, dict_elements(Rest, Line)}.
