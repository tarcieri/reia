%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bytecode).
-export([compile/2, compile/3, load/1, load/2]).
-include("reia_compile_options.hrl").

% Ideally this record is opaque to everything except this module
% No other modules should operate directly on raw Reia bytecode
-record(reia_module, {version=0, filename, base_module}).

% Load the given compiled Reia module, executing its toplevel function
load(Bin) -> load(Bin, []).

% Load the given compiled Reia module, executing its toplevel function with
% the given arguments (in order to pass along a default binding)
load(Bin, Args) ->
	#reia_module{filename = Filename, base_module = Module} = binary_to_term(Bin),
	Name = list_to_atom(Filename),
	code:load_binary(Name, Filename, Module),
	Result = apply(Name, toplevel, Args),
	{ok, Name, Result}.
  
% Compiled evaluation of a parsed Reia file
compile(Filename, Expressions) ->
  compile(Filename, Expressions, #compile_options{}).

compile(Filename, Expressions, Options) ->
  io:format("Output Code: ~p~n", [Expressions]),
  case compile_expressions(Filename, Expressions, Options) of
    {ok, _Module, Bin} ->
      Module = #reia_module{filename=Filename, base_module=Bin},
      {ok, term_to_binary(Module)};
    error ->
      throw({error, "internal Erlang compiler error"})
  end.

% Output raw Erlang bytecode for inclusion into compiled Reia bytecode
compile_expressions(Filename, Expressions, Options) ->
  case Options#compile_options.toplevel_wrapper of
    true ->
      % Build argument list in the abstract format
      Args = [{var, 1, Name} || Name <- []],

      % Create a "toplevel" function which is called when the module is loaded
      Function = {function, 1, toplevel, length(Args), [
        {clause, 1, Args, [], Expressions}
      ]},
  
      Module = [
        {attribute, 1, module, list_to_atom(Filename)},
        {attribute, 1, file, {Filename, 1}},
        {attribute, 1, code, Expressions},
        Function
      ];
    false ->
      {module, _Line, Name, Functions} = Expressions,
      Module = [
        {attribute, 1, module, Name},
        {attribute, 1, file, {Filename, 1}},
        {attribute, 1, code, Expressions}
        |Functions
      ]
  end,
  compile:forms(Module, compile_options(Options)).

compile_options(Options) ->
  ErlOptions = Options#compile_options.erlc_options,
  HiPEAvailable = hipe_available(),
  if
    Options#compile_options.autohipe and HiPEAvailable -> [native|ErlOptions];
    true -> ErlOptions
  end.

hipe_available() ->
  case erlang:system_info(hipe_architecture) of
    undefined -> false;
    _         -> true
  end.
