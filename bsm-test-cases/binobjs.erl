-module(binobjs).
-export([binobjs/3]).

%% From dets_v9:binobjs2terms/6.
binobjs(Bin, _Bin1, ObjSz) ->
    <<_:ObjSz/binary, T/binary>> = Bin,
    <<NObjSz:32, T1/binary>> = T,
    binobjs(T, T1, NObjSz).
