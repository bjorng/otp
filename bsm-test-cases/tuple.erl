-module(tuple).

-export([scan_skip/1]).

bin2bins({a,T} = Bin) ->
    ex:bins_set(T, Bin, Bin).

bins_set(_, A, B) ->
    {A,B}.

scan_skip(_) ->
    KO = {a,b},
    bin2bins(KO).

