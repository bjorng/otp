-module('12').

-export([percent_encode_binary/1]).

percent_encode_binary(<<A:4,Rest/binary>>) ->
    percent_encode_binary(Rest);
percent_encode_binary(<<>>) ->
    ok.

