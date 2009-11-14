%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([transform/1]).
-include("reia_nodes.hrl").

%% Numerical types
transform(Exprs = #integer{}) -> Exprs;
transform(Exprs = #float{})   -> Exprs;

%% Lists
transform(#list{line=Line, elements=Elements}) ->
  list_cons(Elements, Line);

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

%% Generate cons for lists
list_cons([], Line) ->
  {nil, Line};
list_cons([Element|Rest], Line) ->
  {cons, Line, transform(Element), list_cons(Rest, Line)}.
