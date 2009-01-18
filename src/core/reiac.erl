%
% reiac: Command-line compiler for producing .beam files from .re files
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reiac).
-export([file/1,file/2]).

file(Filename) ->
  case filename:extension(Filename) of
    ".re" ->
      file(Filename, filename:basename(Filename, ".re") ++ ".beam");
    Ext ->
      {error, {bad_extension, Ext}}
  end.
  
file(Filename, Outfile) ->
  case file:read_file(Filename) of
    {ok, Data} ->
      case reia_parse:string(binary_to_list(Data)) of
        {ok, [{module, _, _, _}] = Forms} ->
          module(Forms, Outfile);
        {ok, [{class, _, _, _}] = Forms} ->
          module(Forms, Outfile);
        {ok, _Forms} ->
          {error, "compiled Reia must define exactly one module or class"};
        {error, {Line, Message}} ->
          {error, io_lib:format("Line ~w: ~s", [Line, Message])}
      end;
    {error, Err} ->
      {error, io_lib:format("~p", [Err])}
  end.
  
module(Forms, Outfile) ->
  {ok, _Module, Bin} = forms(Forms),
  file:write_file(Outfile, Bin),
  {ok, Outfile}.
  
forms(Forms) ->
  Passes = [case Pass of dynamic -> static; _ -> Pass end || Pass <- reia_compiler:default_passes()],
  ErlForms = reia_compiler:compile(Forms, Passes),
  compile:forms(ErlForms, [
    debug_info, 
    export_all, 
    verbose, 
    report_errors, 
    report_warnings
    %{parse_transform, smart_exceptions}
  ]).