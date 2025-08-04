-module('9').

-export([decode/1]).

decode(<<C,Cs/binary>>, Acc) ->
    decode(Cs, <<Acc/binary,C>>).

decode(Cs) ->
    decode(Cs, <<>>).

