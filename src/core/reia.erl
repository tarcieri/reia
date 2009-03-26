%
% reia: Functions for calling into the Reia environment from Erlang
% Copyright (C)2008-09 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia).
-export([
  apply/3, apply/4,
  invoke/3, invoke/4,
  parse/1,
  spawn/2, spawn/3,
  spawn_link/2, spawn_link/3,
  load_file/1, erl_load/1
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
  reia_erl:r2e(erlang:apply(Constant, Method, [reia_erl:e2r(Arg) || Arg <- Arguments]));
apply(Constant, Method, Arguments, Block) ->
  reia_erl:r2e(erlang:apply(Constant, Method, [reia_erl:e2r(Arg) || Arg <- Arguments ++ [Block]])).
  
% Invoke a method on the given Reia object
invoke(Object, Method, Arguments) ->
  invoke(Object, Method, Arguments, nil).
invoke(Object, Method, Arguments, _Block) ->
  reia_erl:r2e(reia_class:call(Object, {Method, [reia_erl:e2r(Arg) || Arg <- Arguments]})).
  
% Parse a given string (in standard Erlang list form) into its Reia parse tree
%  parse(String) -> {ok, ParseTree} | {error, Reason}
%  Types  String = list()
parse(String) ->
  reia_parse:string(String).
  
% Create a Reia object of the given class, i.e. spawn its gen_server
spawn(Class, Arguments) -> reia:spawn(Class, Arguments, nil).
spawn(Class, Arguments, Block) ->
  reia_class:inst(Class, Arguments, Block).
  
% Create a Reia object of the given class, i.e. spawn its gen_server and link
% its process to this one
spawn_link(Class, Arguments) -> reia:spawn(Class, Arguments, nil).
spawn_link(Class, Arguments, Block) ->
  reia_class:inst(Class, Arguments, Block).
  
% Load the given file through the Reia loader
load_file(File) ->
  reia:apply('Loader', 'start', [File]).

% Load a file when invoked from the Erlang command line
% Handles funky argument thunking coming off the shell
erl_load([File]) ->
  load_file(File).