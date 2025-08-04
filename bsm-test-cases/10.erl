-module('10').

-export([scan_skip/1]).

bin2bins(<<4:32,T/binary>> = Bin) ->
    bins_set(T, Bin, Bin).
%% ,
%%     Bin.

bins_set(_, <<_:8,A/binary>>, <<_:16,B/binary>>) ->
    {A,B}.

scan_skip(<<_:16/binary,KO/binary>>) ->
    bin2bins(KO).

