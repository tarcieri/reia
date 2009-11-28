%
% reia_compiler: Translates Reia into Erlang, then into Erlang bytecode
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([compile/2, compile/3]).
-define(passes, [r2e]).

compile(Filename, Expressions) ->
  compile(Filename, Expressions, []).

compile(Filename, Expressions, Options) ->
  Modules = [list_to_atom("reia_" ++ atom_to_list(Pass)) || Pass <- ?passes],
  Fun = fun(Pass, Exprs) -> [Pass:transform(Expr) || Expr <- Exprs] end,
  Expressions2 = lists:foldl(Fun, Expressions, Modules),
  reia_bytecode:compile(Filename, Expressions2, Options).