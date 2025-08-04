-module(bar).

-export([scan_skip/1]).

bin2bins(<<_:32,Bin/binary>> = KO) ->
    bins_bag(Bin, KO).

bins_bag(<<_:32,T/binary>>, KO) ->
    bins_bag(T, KO).

scan_skip(KO) ->
    bin2bins(KO).

