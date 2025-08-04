-module('11').

-export([segment_fragment_to_pointers/2]).

segment_fragment_to_pointers(_P, <<>>) ->
    [];
segment_fragment_to_pointers(P, <<SzP:8/binary,B/binary>>) ->
    [{P, SzP} | segment_fragment_to_pointers(P + 8, B)].

