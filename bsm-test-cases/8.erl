-module('8').

-export([replace_label/1]).

replace_label(<<"f(",T/binary>>) ->
    replace_label_1(T).

replace_label_1(Lbl0) ->
    Sz = byte_size(Lbl0) - 1,
    case Lbl0 of
	<<Lbl1:Sz/bytes,")">> ->
	    ok
    end.

