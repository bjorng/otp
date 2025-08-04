-module('18').

-export([mpint/1]).

mpint(_I) ->
    <<_,V/binary>> = e:f(),
    <<(byte_size(V)):32,V/binary>>.

