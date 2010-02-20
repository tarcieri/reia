%
% reia_methods: Trying to add state to a language which goes out of its way to
%               avoid it is really hard!  "Local" method calls inside of 
%               objects need to pass around instance variables as well.
%               Worse we need knowledge of the binding to distinguish calls to
%               lambads from local method calls, so passing around the ivars
%               to local method calls needs its own pass after the SSA 
%               transform.  You can think of this as a continuation pass from
%               reia_ivars, with a little bit of help from reia_ssa.
% 
% Copyright (C)2008 Tony Arcieri 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_methods).
-export([ast/1, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, void, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.
  
%% Method calls
transform(_, {method_call, Line, Method, Arguments, Block, IvarsIn, IvarsOut}) ->
  Nonce = list_to_atom("__return_value_" ++ reia_compiler:nonce()),
  Node = {block, Line, [
    {match, Line, 
      return_value_pattern(Line, IvarsOut, Nonce), 
      method_invocation(Line, Method, Arguments, Block, IvarsIn)
    },
    {identifier, Line, Nonce}
  ]},
  {walk, void, Node};
  
% Walk unrecognized nodes without transforming them
transform(_, Node) ->
  {walk, void, Node}.
  
% Pattern for matching a method return value 
% (same format as gen_server handle_call)
return_value_pattern(Line, IvarsOut, Nonce) ->
  {identifier, _Line, IvarsName} = IvarsOut,
  {erl_forms, Line,
    {tuple, Line, [
      {atom, Line, reply},
      {tuple, Line, [
        {atom, Line, ok},
        {var, Line, Nonce}
      ]}, 
      {var, Line, IvarsName}
    ]}
  }.
  
% Invoke the given method
% Dispatches to dispatch_method({Method, Args}, Caller, State)
method_invocation(Line, Method, Arguments, Block, IvarsIn) ->
  {identifier, Line, Name} = Method,
  Arguments2 = [
    {tuple, Line, [
      {atom, Line, Name},
      form_list(Arguments, Line),
      Block
    ]},
    {atom, Line, local}, 
    forms(IvarsIn)
  ],
  {erl_forms, Line, {call, Line, {atom, Line, dispatch_method}, Arguments2}}.
  
% Ugh, hacka perform the final passes of the compiler.
% FIXME: This sucks
forms(Node) ->
  reia_r2e:forms(Node).
form_list(Node, Line) ->
  reia_r2e:list_to_forms(Node, Line).