-module(reiac).
-export([file/1,file/2]).

file(Filename) ->
  case filename:extension(Filename) of
    ".ra" ->
      file(Filename, filename:basename(Filename, ".ra") ++ ".beam");
    Ext ->
      {error, {bad_extension, Ext}}
  end.
  
file(Filename, Outfile) ->
  {ok, Data} = file:read_file(Filename),
  {ok, Scanned, _} = reia_scan:scan(binary_to_list(Data)),
  {ok, Parsed} = reia_parse:parse(Scanned),
  {ok, _Module, Bin} = forms(Parsed),
  file:write_file(Outfile, Bin),
  {ok, Outfile}.
  
forms(Forms) ->
  ErlForms = reia_compiler:compile(Forms),
  compile:forms(ErlForms, [debug_info, export_all, verbose, report_errors, report_warnings]).