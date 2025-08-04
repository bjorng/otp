-module(reduced).

-export([replace_label/1]).

replace_label_1(Lbl0) ->
    Sz = byte_size(Lbl0) - 1,
    case Lbl0 of
        <<_:Sz/bytes,")">> ->
            [maps:get(), ")"];
        _ ->
            Lbl0
    end.

replace_label(<<"f(",T/binary>>) ->
    replace_label_1(T).

