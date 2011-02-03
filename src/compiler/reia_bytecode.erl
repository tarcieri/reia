%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009-10 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bytecode).
-export([compile/2, compile/3, load_file/1, load/1, load/2]).
-include("reia_nodes.hrl").
-include("reia_compile_options.hrl").

% Ideally this record is opaque to everything except this module
% No other modules should operate directly on raw Reia bytecode
-record(reia_module, {version=0, filename, base_module}).

% Load the given file, executing its toplevel function
load_file(Filename) -> 
  {ok, Bin} = file:read_file(Filename),
  load(Bin).

% Load the given compiled Reia module, executing its toplevel function
load(Bin) -> load(Bin, []).

% Load the given compiled Reia module, executing its toplevel function with
% the given arguments (in order to pass along a default binding)
load(Bin, Args) ->
	#reia_module{filename = Filename, base_module = Module} = binary_to_term(Bin),
	Name = list_to_atom(filename:rootname(Filename)),
	code:load_binary(Name, Filename, Module),
	Result = apply(Name, toplevel, Args),
	{ok, Name, Result}.
  
% Compiled evaluation of a parsed Reia file
compile(Filename, Exprs) ->
  compile(Filename, Exprs, #compile_options{}).

compile(Filename, Exprs, Options) ->  
  case compile_expressions(Filename, Exprs, Options) of
    {ok, _Module, Bin} ->
      Module = #reia_module{filename=Filename, base_module=Bin},
      {ok, term_to_binary(Module)};
    error ->
      throw({error, "internal Erlang compiler error"})
  end.

% Output raw Erlang bytecode for inclusion into compiled Reia bytecode
compile_expressions(Filename, Exprs, Options) ->  
  {Module, Submodules} = case Options#compile_options.toplevel_wrapper of
    true  -> wrapped_module(Filename, Exprs);
    false -> unwrapped_module(Exprs)
  end,
    
  Header = module_header(Module#module.name, Filename, Module#module.attrs, Options),
  Submodules2 = compile_submodules(Submodules, Filename, Options),
  SubmoduleAttr = {attribute, 1, submodules, Submodules2},
  ErlModule = lists:flatten([Header, SubmoduleAttr, Module#module.exprs]),
  
  % Trying to debug the Reia compiler and want to see the raw code that's
  % getting passed to the Erlang compiler? Uncomment the lines below.
  % io:format("--------------- Reia Compiler Output ------------~n"),
  % io:format("~p~n", [ErlModule]),
  % io:format("----------- End of Reia Compiler Output ---------~n"),
  
  compile:forms(ErlModule, compile_options(Options)).
  
compile_submodules(Submodules, Filename, Options) ->
  [compile_submodule(Submodule, Filename, Options) || Submodule <- Submodules].
  
compile_submodule(Module, Filename, Options) ->
  Header = module_header(Module#module.name, Filename, Module#module.attrs, Options),
  ErlModule = Header ++ Module#module.exprs,

  {ok, Name, Bin} = compile:forms(ErlModule, compile_options(Options)),
  {static, Name, Bin}.

module_header(Name, Filename, CustomAttrs, Options) ->
  ParentAttr = {attribute, 1, parent, list_to_atom(filename:rootname(Filename))},
  ErlAttrs = lists:map(
    fun({AttrName, Value}) -> {attribute, 1, AttrName, Value} end,
    CustomAttrs
  ),
    
  [
    {attribute, 1, module, Name},
    {attribute, 1, file, {Filename, 1}},
    {attribute, 1, code, Options#compile_options.code},
    ParentAttr|ErlAttrs
  ].
  
wrapped_module(Filename, Exprs) ->
  Name = list_to_atom(filename:rootname(Filename)),
  {ok, Exprs2, Submodules} = reia_modules:replace(Exprs, fun module_loader/1),
  Function = {function, 1, toplevel, 0, [
    {clause, 1, [], [], Exprs2}
  ]},
  Module = #module{line=1, name=Name, exprs=[Function]},
  {Module, Submodules}.
  
unwrapped_module(Exprs) ->
  case Exprs of
    [#module{} = Module] -> 
      {ok, Functions2, Submodules} = reia_modules:replace(Module#module.exprs, fun module_loader/1),
      {Module#module{exprs=Functions2}, Submodules};
    _ ->
      throw({error, "code without a toplevel wrapper should define exactly one module"})
  end.

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
  
module_loader(Module) ->
  Name = Module#module.name,
  {call,1,
    {remote,1,{atom,1,reia_internal},{atom,1,load_submodule}},
    [{atom,1,Name},{call,1,{atom,1,module_info},[{atom,1,attributes}]}]
  }.