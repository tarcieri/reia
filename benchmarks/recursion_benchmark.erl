-module (recursion_benchmark).

-export ([run/0]).

run() ->
    loop(1000000).
    
loop(0) ->
    ok;
    
loop(N) ->
    loop(N - 1).
