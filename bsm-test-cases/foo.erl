-module(foo).

-export([b/1, c/2]).

b(<<A:32,B:16>>) ->
    {A,B}.

c(N, Bin) ->
    <<H:N,T/binary>> = Bin,
    {H,T}.
