%
% reia_comparisons: Handle special cases of the comparison operator "=="
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_comparisons).
-export([transform/2]).
-include("reia_nodes.hrl").
-define(string_match(Line, Name), #tuple{line=Line, elements=[
  #atom{line=Line, name='reia_string'}, 
  #var{line=Line, name=Name}
]}).
-define(string_to_binary(Line, Name), #native_call{
  line=Line, 
  module=erlang, 
  function=iolist_to_binary, 
  args=[#var{line=Line, name=Name}]
}).

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform/1, Exprs).
  
% The == operator
transform(#binary_op{line=Line, type='==', left=Left, right=Right}) ->
  #'case'{line=Line, expr=#tuple{line=Line, elements=[Left, Right]}, clauses=[
    #clause{line=Line, patterns=[#tuple{line=Line, elements=[
        ?string_match(Line, '__left'), 
        ?string_match(Line, '__right')
      ]}], exprs=[
        #binary_op{
          line  = Line,
          type  = '==',
          left  = ?string_to_binary(Line, '__left'),
          right = ?string_to_binary(Line, '__right')
        }
    ]},
    #clause{line=Line, patterns=[#tuple{line=Line, elements=[
        #var{line=Line, name='__left'},
        #var{line=Line, name='__right'}
      ]}], exprs=[
      #binary_op{
        line  = Line, 
        type  = '==', 
        left  = #var{line=Line, name='__left'}, 
        right = #var{line=Line, name='__right'}
      }
    ]}
  ]};
  
% The != operator, translated to a unary not and a == operator
transform(#binary_op{line=Line, type='!=', left=Left, right=Right}) ->
  Node = #binary_op{line=Line, type='==', left=Left, right=Right},
  transform(#unary_op{line=Line, type='not', expr=Node});
  
% Walk unrecognized nodes without transforming them
transform(Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node).