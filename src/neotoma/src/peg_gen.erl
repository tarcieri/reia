-module(peg_gen).
-author("Sean Cribbs <seancribbs@gmail.com>").
-export([file/1, file/2, bootstrap/0]).

%% @doc Generates a parser from the specified file.
%% @equiv file(Filename, [])
%% @spec file(Filename::string()) -> ok
file(InputGrammar) ->
  file(InputGrammar, []).

%% @doc Generates a parser from the specified file.
%% <pre>    Options = [Option] <br />
%%   Option = {module, OutputModuleName::atom()} | <br/>             {output, OutputDirectory::string()} | <br />             {transform_module, TransformModuleName::atom()} </pre>
%% @spec file(Filename::string(), Options) -> ok
file(InputGrammar, Options) ->
  Basename = filename:basename(InputGrammar, ".peg"),
  InputDir = filename:dirname(InputGrammar),
  ModuleName = proplists:get_value(module, Options, list_to_atom(Basename)),
  OutputDir = proplists:get_value(output, Options, InputDir),
  OutputFilename = filename:join(OutputDir, atom_to_list(ModuleName) ++ ".erl"),
  TransformModule = proplists:get_value(transform_module, Options, false),
  validate_params(filename:absname(InputGrammar),
                  ModuleName,
                  TransformModule,
                  filename:absname(OutputFilename)),
  ModuleAttrs = generate_module_attrs(ModuleName),
  Rules = parse_grammar(InputGrammar),
  TransformFun = create_transform(TransformModule, OutputDir),
  file:write_file(OutputFilename, [ModuleAttrs, "\n", Rules, "\n", TransformFun]).

validate_params(InputGrammar, _, _, OutputFile) when InputGrammar =:= OutputFile ->
  throw({badarg, "Input and output file are the same!"});
validate_params(_,ModName,_,_) when not is_atom(ModName) ->
  throw({badarg, "Output module name is not an atom!"});
validate_params(_,_, false, _) -> ok;
validate_params(_,_, TransformModule, _) when not is_atom(TransformModule) ->
  throw({badarg, "transform_module option must be an atom"});
validate_params(_,Basename, TransformModule, _) when Basename =:= TransformModule ->
  throw({badarg, "Transform module named same as parser module!"});
validate_params(_,_, TransformModule, OutputFile) ->
  OutMod = list_to_atom(filename:basename(OutputFile, ".erl")),
  case OutMod of
    TransformModule -> throw({badarg, "Transform module file same as parser output file!"});
    _ -> ok
  end.

generate_module_attrs(ModName) ->
  ["-module(",atom_to_list(ModName),").\n",
   "-export([parse/1,file/1]).\n",
   "-include_lib(\"neotoma/include/peg.hrl\").\n"].

parse_grammar(InputFile) ->
  case peg_meta:file(InputFile) of
    {fail, Index} ->
      throw({grammar_error, {fail, Index}});
    {Parsed, Remainder, Index} ->
      io:format("WARNING: Grammar parse ended unexpectedly at ~p, generated parser may be incorrect.~nRemainder:~n~p",
                [Index, Remainder]),
      Parsed;
    L when is_list(L) -> L;
    _ -> throw({error, {unknown, grammar, InputFile}})
  end.

create_transform(false,_) ->
  "transform(_,Node) -> Node.";
create_transform(ModName,Dir) when is_atom(ModName) ->
  XfFile = filename:join(Dir, atom_to_list(ModName) ++ ".erl"),
  case filelib:is_regular(XfFile) of
    true -> io:format("'~s' already exists, skipping generation.~n", [XfFile]);
    false -> generate_transform_stub(XfFile, ModName)
  end,
  ["transform(Symbol,Node) -> ",atom_to_list(ModName),":transform(Symbol, Node)."].

generate_transform_stub(XfFile,ModName) ->
  Data = ["-module(",atom_to_list(ModName),").\n",
         "-export([transform/2]).\n\n",
         "%% Add clauses to this function to transform syntax nodes\n",
         "%% from the parser into semantic output.\n",
         "transform(Symbol, Node) when is_atom(Symbol) ->\n  Node."],
  file:write_file(XfFile, Data).

%% @doc Bootstraps the neotoma metagrammar.  Intended only for internal development!
%% @equiv file("src/peg_meta.peg", [{transform_module, peg_meta_gen}])
bootstrap() ->
  file("src/peg_meta.peg", [{transform_module, peg_meta_gen}]).
