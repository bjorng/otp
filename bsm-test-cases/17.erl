-module('17').

-export([live_past_call_mctx/1]).

live_past_call_mctx_1(<<_:32,Bin/binary>> = KO) ->
    live_past_call_mctx_2(<<4:8>>, KO),
    <<_:8,Rest/binary>> = e:f(),
    live_past_call_mctx_2(Bin, Rest).

live_past_call_mctx_2(<<_:32,T/binary>>, KO) ->
    live_past_call_mctx_2(T, KO);
live_past_call_mctx_2(<<>>, _) ->
    ok.

live_past_call_mctx(<<_:32,KO/binary>>) ->
    live_past_call_mctx_1(KO).

