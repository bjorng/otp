-module('7').

-export([replace_label/1]).

replace_label(<<"f",T/binary>>) ->
    ex:f(T).

