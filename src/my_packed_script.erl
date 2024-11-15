-module(my_packed_script).

-export([main/1]).

main([]) ->
    io:format("hello, world\n", []),
    ok.
