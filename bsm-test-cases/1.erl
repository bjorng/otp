-module(reduced).

-export([replace_label/1]).

replace_label(<<"(",T/binary>>) ->
    replace_label_1(T).

replace_label_1(Lbl0) ->
    case Lbl0 of
	<<"0)">> ->
	    x;
	<<_:4/bytes,")">> ->
	    []
    end.

