-module('13').

-export([rejoin_atoms/1]).

rejoin_atoms([<<"`'",Tail/binary>> | Ops]) ->
    Sz = byte_size(Tail) - 2,
    case Tail of
        <<_:Sz/bytes,"'`">> ->
            rejoin_atoms(Ops)
    end.

