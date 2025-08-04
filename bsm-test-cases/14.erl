-module('14').

-export([parse_pubid_literal/2]).

parse_pubid_literal(<<Stop,Rest/binary>>, Stop) ->
    Rest;
parse_pubid_literal(<<_,Rest/binary>>, Stop) ->
    parse_pubid_literal(Rest, Stop).

