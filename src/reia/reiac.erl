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
  case parse(Filename) of
    {ok, Forms = [{module, _, _, _}]} ->
      {ok, _Module, Bin} = forms(Forms),
      file:write_file(Outfile, Bin),
      {ok, Outfile};
    {ok, _} ->
      {error, "compiled Reia must define exactly one module"};
    Err ->
      Err
  end.
  
parse(Filename) ->
  case file:read_file(Filename) of
    {ok, Data} ->
      case reia_scan:scan(binary_to_list(Data)) of
        {ok, Scanned, _} -> 
          reia_parse:parse(Scanned);
        Err ->
          Err
      end;
    Err ->
      Err
  end.
  
forms(Forms) ->
  ErlForms = reia_compiler:compile(Forms),
  compile:forms(ErlForms, [debug_info, export_all, verbose, report_errors, report_warnings]).