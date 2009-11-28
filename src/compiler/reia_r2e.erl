%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([transform/1]).
-include("reia_nodes.hrl").

%% Terminals
transform(#integer{} = Expr) -> Expr;
transform(#float{} = Expr)   -> Expr;
transform(#atom{} = Expr)    -> Expr;
transform(#identifier{line=Line, name=Name}) -> {var, Line, Name};

%% Matches
transform(#match{line=Line, left=Left, right=Right}) ->
  {match, Line, transform(Left), transform(Right)};

%% Lists
transform(#cons{line=Line, expr=Expr, tail=Tail}) ->
  {cons, Line, transform(Expr), transform(Tail)};

transform(#empty{line=Line}) ->
  {nil, Line};

%% Tuples
transform(#tuple{line=Line, elements=Exprs}) ->
  {tuple, Line, [transform(Expr) || Expr <- Exprs]};

%% Operators
transform(#unary_op{line=Line, type=Type, val=Val}) ->
  {op, Line, Type, transform(Val)};

transform(#binary_op{line=Line, type='**', val1=Val1, val2=Val2}) ->
  {call, Line,
    {remote, Line, {atom, Line, math}, {atom, Line, pow}},
    [transform(Val1), transform(Val2)]
  };

transform(#binary_op{line=Line, type=Type, val1=Val1, val2=Val2}) ->
  {op, Line, Type, transform(Val1), transform(Val2)}.
