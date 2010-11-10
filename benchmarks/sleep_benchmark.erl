-module (sleep_benchmark).

-export ([run/0]).


run() ->
    timer:sleep(1000).
