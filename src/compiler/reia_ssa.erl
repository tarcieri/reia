-module(reia_ssa).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_compile_options.hrl").
-include("reia_bindings.hrl").

% Lists of expressions
transform(Exprs, Options) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs, Options#compile_options.scope),
  reia_bindings:revert(BAExprs).