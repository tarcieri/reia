-module(reia_ssa).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_compile_options.hrl").
-include("reia_bindings.hrl").

% Lists of expressions
transform(Exprs, Options) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs, Options#compile_options.scope),
  reia_syntax:map_subtrees(fun transform_node/1, BAExprs).

transform_node(#bindings{node=Node}) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node);
transform_node(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).