
-module(exceptions).
-compile([export_all, pure_guards]).
%-compile([all_pure]).

-include_lib("eunit/include/eunit.hrl").

-define(SIZE, 42).

%% FIXME:
%% length succeeds but len raises badarg.
%% Need to actually wrap guard calls in try..catch or sth.

f(A) when len(A) < ?SIZE -> ok;
f(_) -> error.

len(L) -> length(L).

f_test_() ->
    [?_assertMatch(ok, f([]))
    ,?_assertMatch(error, f(lists:duplicate(?SIZE, x)))
    ,?_assertMatch(error, f(not_list))
].
