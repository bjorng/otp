-module('15').

-export([precomp_repl/1]).

precomp_repl(<<$\\,$g,${,Rest/binary>>) when byte_size(Rest) > 0 ->
    {_, <<$},_/binary>>} = pick_int(Rest),
    ok.

pick_int(Bin) ->
    {[], Bin}.

