%
% parser_ab: A/B test driver for the Reia PEG parser
% Copyright (C)2011 Graeme Defty
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%
-module(parser_ab).
-include_lib("kernel/include/file.hrl").
-include("../../src/compiler/reia_nodes.hrl").
 
-export([run/0]).


run()->
    walk_dir(".", fun testfile/1).

walk_dir(Dir, Proc) ->
    {ok,Files} = file:list_dir(Dir),
    dofiles(Dir, Proc, lists:sort(Files)).

dofiles(_  ,    _, []   ) -> ok;
dofiles(Dir, Proc, [H|T]) ->
    File = Dir ++ ["/"|H],
    Filetype = 
        case filelib:is_regular(File) of
            true -> file;
            false ->
	            case file:read_link_info(File) of
		            {ok, #file_info{type = symlink}} -> symlink;
		            _ -> directory
		        end
		end,
    dofile(Filetype, Dir, Proc, File),
    dofiles(Dir, Proc, T).

dofile(symlink, _, _, File) -> ok;

dofile(directory, _, _   , ".")  -> ok;
dofile(directory, _, _   , "..") -> ok;
dofile(directory, _, Proc, File) ->
    walk_dir(File, Proc);

dofile(file, _, Proc, File) ->
    Proc(File).

testfile(File) ->
    case string:right(File,3) == ".re" of
        true ->
            runtest(File);
        false ->
            ok
    end.
        

runtest(Mod) ->
    io:format("Testing ~s~n",[Mod]),
    {ok,BinSource} = file:read_file(Mod),
    Source = binary_to_list(BinSource),
%    io:format("Source is :~n~s~n",[Source]),
    case reia_yecc_parse:string(Source) of
        {ok,Rslt1} ->
            Rslt2 = reia_parse:parse(Source),
%            io:format("Result 1 is :~n~p~n",[Rslt1]),
%            io:format("Result 2 is :~n~p~n",[Rslt2]),
            case compare(Rslt1,Rslt2) of
                ok ->
                    "Success";
                Comp ->
                    io:format("=====> Result is ~p~n",[Comp]),
                    throw("Problem!!!!!")
            end;
        Err ->
            Rslt2 = reia_parse:parse(Source),
            io:format("~p~n",[Rslt2]),
            io:format("==> leex/yecc failed compiling ~s~n",[Mod]),
            io:format("==>    ~p~n",[Err]),
            io:format("==> PEG parser gave the result above. ~n",[]),
            io:get_line("==> Press enter to continue.")
    end.


compare(R1,R2) ->
    case comp(R1,R2) of
        ok    ->  ok;
        Terms ->  Terms
    end.

comp([],[])       ->  ok;
comp([H1|_T1],[]) ->  {H1,nil};
comp([],[H2|_T2]) ->  {nil,H2};

comp([H1|T1],[H2|T2]) ->
    case comp(H1,H2) of
        ok ->
            case comp(T1,T2) of
                ok -> ok;
                Terms -> [H1|Terms]
            end;
        Terms ->
            Terms
    end;

comp(R1=#'case'{},R2=#'case'{}) ->
    case R1#'case'.line == R2#'case'.line of
      true -> 
        case comp(R1#'case'.expr, R2#'case'.expr) of
          ok -> 
            case comp(R1#'case'.clauses, R2#'case'.clauses) of
              ok    -> ok;
              Terms -> [{'case',R1#'case'.line}|Terms]
            end;
          false -> [{'case',R1#'case'.line}]
        end;
      false -> [{'case',R1#'case'.line}]
    end;

comp(R1=#class{},R2=#class{}) ->
    case R1#class.line == R2#class.line of
      true -> 
        case R1#class.name == R2#class.name of
          true -> 
            case R1#class.name == R2#class.name of
              true -> 
                case R1#class.parent == R2#class.parent of
                  true -> 
                    case comp(R1#class.exprs, R2#class.exprs) of
                      ok    -> ok;
                      Terms -> [{class,R1#class.line}|Terms]
                    end;
                  false -> [{class,R1#class.line}]
                end;
              false -> [{class,R1#class.line}]
            end;
          false -> [{class,R1#class.line}]
        end;
      false -> [{class,R1#class.line}]
    end;

comp(R1=#clause{},R2=#clause{}) ->
    case R1#clause.line == R2#clause.line of
      true -> 
        case comp(R1#clause.patterns, R2#clause.patterns) of
          ok -> 
            case comp(R1#clause.exprs, R2#clause.exprs) of
              ok    -> ok;
              Terms -> [{clause,R1#clause.line}|Terms]
            end;
          false -> [{clause,R1#clause.line}]
        end;
      false -> [{clause,R1#clause.line}]
    end;

comp(R1=#function{},R2=#function{}) ->
    case R1#function.line >= R2#function.line of
      true -> 
        case R1#function.name == R2#function.name of
          true -> 
            case comp(R1#function.args,R2#function.args) of
              ok -> 
                case comp(R1#function.block, R2#function.block) of
                  ok -> 
                    case comp(R1#function.body, R2#function.body) of
                      ok  -> ok;
                      Terms -> [{function,R1#function.line}|Terms]
                    end;
                  Terms -> [{function,R1#function.line}|Terms]
                end;
              Terms -> [{function,R1#function.line}|Terms]
            end;
          false -> [{function,R1#function.line,name,R1#function.name,R1#function.name}]
        end;
      false -> [{function,R1#function.line,line,R1#function.line,R2#function.line}]
    end;

comp(R1=#module{},R2=#module{}) ->
    case R1#module.line == R2#module.line of
      true -> 
        case R1#module.name == R2#module.name of
          true -> 
            case R1#module.name == R2#module.name of
              true -> 
                case comp(R1#module.exprs, R2#module.exprs) of
                  ok    -> ok;
                  Terms -> [{module,R1#module.line}|Terms]
                end;
              false -> [{module,R1#module.line}]
            end;
          false -> [{module,R1#module.line}]
        end;
      false -> [{module,R1#module.line}]
    end;

comp(R1=#nil{},R2=#nil{}) ->
    ok;

comp(#var{name='_'},#var{name='_'}) ->
    ok;


comp(R1,R2) when R1 == R2 -> ok;
comp(R1,R2) ->
    io:format("~n=====> Unknown Match!!!!~n~n~p~n~n~p~n~n",[R1, R2]),
    throw("FUNNY STUFF!!!"),
    ok.


