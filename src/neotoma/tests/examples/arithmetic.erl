-module(arithmetic).
-export([parse/1, file/1]).
-include("../../include/peg_i.hrl").

file(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  parse(binary_to_list(Bin)).

parse(Input) ->
  peg:setup_memo(arithmetic),
  Result = case additive(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} ->
                AST;
             Any -> Any
           end,
  peg:release_memo(),
  Result.

additive(Input, Index) ->
  peg:p(Input, Index, additive, fun(I,D) ->
                             (peg:choose([peg:seq([fun multitive/2,
                                                  peg:string("+"),
                                                  fun additive/2]),
                                         fun multitive/2]))(I,D) end,
       fun(Node, Idx) -> transform(additive, Node, Idx) end).

multitive(Input, Index) ->
  peg:p(Input, Index, multitive, fun(I,D) ->
                              (peg:choose([peg:seq([fun primary/2,
                                                   peg:string("*"),
                                                   fun multitive/2]),
                                          fun primary/2]))(I,D)
                          end,
       fun(Node, Idx) -> transform(multitive, Node, Idx) end).

primary(Input, Index) ->
  peg:p(Input, Index, primary, fun(I,D) ->
                            (peg:choose([peg:seq([peg:string("("),
                                                 fun additive/2,
                                                 peg:string(")")]),
                                        fun decimal/2]))(I,D)
                        end,
       fun(Node, Idx) -> transform(primary, Node, Idx) end).

decimal(Input, Index) ->
  peg:p(Input, Index, decimal, fun(I,D) ->
                            (peg:charclass("[0-9]"))(I,D)
                        end,
       fun(Node, Idx) -> transform(decimal, Node, Idx) end).

%% Transform the nodes into the result of the expression
transform(decimal, Node, _Idx) ->
  list_to_integer([Node]);
transform(primary, Node, _Idx) when is_integer(Node) ->
  Node;
transform(primary, Node, _Idx) when is_list(Node) ->
  lists:nth(2, Node);
transform(multitive, Node, _Idx) when is_integer(Node) ->
  Node;
transform(multitive, Node, _Idx) when is_list(Node) ->
  hd(Node) * lists:nth(3, Node);
transform(additive, Node, _Idx) when is_integer(Node) ->
  Node;
transform(additive, Node, _Idx) when is_list(Node) ->
  hd(Node) + lists:nth(3, Node).
