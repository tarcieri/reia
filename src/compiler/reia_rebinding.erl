-module(reia_rebinding).
-export([transform/2]).
-include("reia_nodes.hrl").

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform_node/1, Exprs).

transform_node(#binary_op{line=Line, type='+=', val1=Left, val2=Right}) ->
  rebind_op(Line, '+', Left, Right);
transform_node(#binary_op{line=Line, type='-=', val1=Left, val2=Right}) ->
  rebind_op(Line, '-', Left, Right);
transform_node(#binary_op{line=Line, type='*=', val1=Left, val2=Right}) ->
  rebind_op(Line, '*', Left, Right);
transform_node(#binary_op{line=Line, type='/=', val1=Left, val2=Right}) ->
  rebind_op(Line, '/', Left, Right);
transform_node(#binary_op{line=Line, type='**=', val1=Left, val2=Right}) ->
  rebind_op(Line, '**', Left, Right);
transform_node(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).

rebind_op(Line, Type, Left, Right) ->
  #match{
    line=Line,
    left=Left,
    right=#binary_op{line=Line, type=Type, val1=Left, val2=Right}
  }.