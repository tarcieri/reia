%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009 Tony Arcieri
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
	Name = list_to_atom(Filename),
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
    true  -> wrapped_module(list_to_atom(Filename), Exprs);
    false -> unwrapped_module(Exprs)
  end,
  
  Submodules2 = compile_submodules(Submodules, Filename, Options),
  
  Header = module_header(Module#module.name, Filename, Options),
  ParentAttr = {attribute, 1, parent, list_to_atom(Filename)},
  SubmoduleAttr = {attribute, 1, submodules, Submodules2},
  ErlModule = lists:flatten([Header, ParentAttr, SubmoduleAttr, Module#module.functions]),
  
  compile:forms(ErlModule, compile_options(Options)).
  
compile_submodules(Submodules, Filename, Options) ->
  [compile_submodule(Submodule, Filename, Options) || Submodule <- Submodules].
  
compile_submodule(Module, Filename, Options) ->
  Header = module_header(Module#module.name, Filename, Options),
  ParentAttr = {attribute, 1, parent, list_to_atom(Filename)},
  ErlModule = lists:flatten([Header, ParentAttr, Module#module.functions]),
  {ok, Name, Bin} = compile:forms(ErlModule, compile_options(Options)),
  {static, Name, Bin}.

module_header(Name, Filename, Options) ->
  [
    {attribute, 1, module, Name},
    {attribute, 1, file, {Filename, 1}},
    {attribute, 1, code, Options#compile_options.code}  
  ].
  
wrapped_module(Name, Exprs) ->
  {ok, Exprs2, Submodules} = reia_modules:replace(Exprs, fun module_loader/1),
  Function = {function, 1, toplevel, 0, [
    {clause, 1, [], [], Exprs2}
  ]},
  Module = #module{line=1, name=Name, functions=[Function]},
  {Module, Submodules}.
  
unwrapped_module(Exprs) ->
  case Exprs of
    [{module, Line, Name, Functions}] -> 
      {ok, Functions2, Submodules} = reia_modules:replace(Functions, fun module_loader/1),
      Module = #module{line=Line, name=Name, functions=Functions2},
      {Module, Submodules};
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
    {remote,1,{atom,1,reia},{atom,1,load_submodule}},
    [{atom,1,Name},{call,1,{atom,1,module_info},[{atom,1,attributes}]}]
  }.