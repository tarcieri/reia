%
% reia_rebinding: Support for operations that alter local variable bindings
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_rebinding).
-export([transform/2]).
-include("reia_nodes.hrl").

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform_node/1, Exprs).

% Transform nodes that potentially rebind variables
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
transform_node(#match{left=Left} = Node) ->
  Node#match{left=reia_syntax:map_subtrees(fun transform_setters/1, [Left])};
transform_node(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).

% Transform shorthand operations that rebind a variable
rebind_op(Line, Type, Left, Right) ->
  #match{
    line=Line,
    left=Left,
    right=#binary_op{line=Line, type=Type, val1=Left, val2=Right}
  }.

% Transform expressions that behave differently in match scope
transform_setters(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).