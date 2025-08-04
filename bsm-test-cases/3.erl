-module(reduced).

-export([rename_mod_1/3]).

rename_mod_2(Subject, Pat, Replacement) ->
    Sz = byte_size(Pat),
    case Subject of
        <<Pat:Sz/bytes,Tail/binary>> ->
            <<Replacement/binary,Tail/binary>>;
        _ ->
            Subject
    end.

rename_mod_1(Ops, Pat, Replacement) ->
    [ rename_mod_2(Op, Pat, Replacement) || Op <- Ops ].

