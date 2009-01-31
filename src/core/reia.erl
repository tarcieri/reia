-module(reia).
-export([
  apply/3, apply/4,
  parse/1
]).

% Call the given Reia function from the Erlang environment, automatically 
% converting methods from Erlang to Reia form.  Automatically assigns the block
% to nil unless it's explicitly passed otherwise
apply(Constant, Method, Arguments) ->
  apply(Constant, Method, Arguments, nil).
  
% Call the given Reia function from the Erlang environment, automatically
% converting methods from Erlang to Reia from.
% FIXME should disintermediate the block from other formal arguments.
apply(Constant, Method, Arguments, nil) ->
  erlang:apply(Constant, Method, [reia_erl:e2r(Arg) || Arg <- Arguments]);
apply(Constant, Method, Arguments, Block) ->
  erlang:apply(Constant, Method, [reia_erl:e2r(Arg) || Arg <- Arguments ++ [Block]]).
  
% Parse a given string (in standard Erlang list form) into its Reia parse tree
%  parse(String) -> {ok, ParseTree} | {error, Reason}
%  Types  String = list()
parse(String) ->
  reia_parse:string(String).