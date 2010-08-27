-module (forloops_benchmark).

-export ([run/0]).

run() ->
    % TODO: a better way to test loop?
    List = lists:seq(0, 1000000),
    loop(List).
    
loop([], Result) ->
    Result;

loop([H|T], Result) ->
    loop(T, [H | Result]).

loop([H|T]) ->
    loop(T, [H]).