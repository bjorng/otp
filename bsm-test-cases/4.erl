-module('4').

-export([unix_pathtype/1]).

unix_pathtype(<<$/,_/binary>>) ->
    absolute;
unix_pathtype([$/ | _]) ->
    absolute.

