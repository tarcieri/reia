%
% reia_ssa: Convert destructive assignments into Static Single Assignment form
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_compile_options.hrl").
-include("reia_bindings.hrl").

% Lists of expressions
transform(Exprs, Options) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs, Options#compile_options.scope),
  reia_syntax:map_subtrees(fun transform_node/1, BAExprs).

transform_node(#bindings{node=#identifier{line=Line, name=Name}=Node, entries=Bindings}) ->
  case dict:find(Name, Bindings) of
    {ok, Version} ->
      #identifier{line=Line, name=ssa_name(Name, Version)};
    error ->
      case Name of
        '_' -> 
          Node;
        _   ->
          throw({error, {Line, lists:flatten(io_lib:format("unbound variable: '~s'", [Name]))}})
      end
  end;
transform_node(#bindings{node=Node}) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node);
transform_node(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).

% Generate the SSA name for a given variable, which takes the form name_version
ssa_name(Name, Version) ->
  Name2 = lists:flatten(io_lib:format("~s_~w", [Name, Version])),
  list_to_atom(Name2).