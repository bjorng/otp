-module(muses).
-export([muses/1]).

%% Currently generates SSA code that uses a variable that
%% is not guaranteed to be defined.
%%
%% Extracted from bs_match_SUITE.

multiple_uses_cmp(<<Y:16>>, <<Y:16>>) -> true;
multiple_uses_cmp(<<_:16>>, <<_:16>>) -> false.

muses(<<_:16,Tail/binary>>) ->
    multiple_uses_cmp(Tail, Tail).
