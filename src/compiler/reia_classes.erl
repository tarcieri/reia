%
% reia_classes: Convert OOP features to Reia code
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_classes).
-export([transform/2]).
-include("reia_nodes.hrl").

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform/1, Exprs).

transform(#class{} = Node) ->
  reia_syntax:map_subtrees(fun transform/1, transform_class(Node));
      
transform(Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node).
    
transform_class(#class{line=Line, name=Name, methods=Funcs}) ->
  #class{line=Line, name=Name, methods=Funcs}.