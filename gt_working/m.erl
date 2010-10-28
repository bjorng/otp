
-module(m).
-compile([export_all,pure_guards]).
%-compile([all_pure]).

-include_lib("eunit/include/eunit.hrl").

n(A, B) when ?MODULE:ng1(A,B,A) ->
    case get(key) of
        undefined -> ok;
        Value when ?MODULE:ng2({A,B}) -> {error1,Value};
        Value when ?MODULE:ng3(A,B) -> {error2,Value};
        Other -> Other
    end.

ng1(A, B, A) -> is_integer(B);
ng1(_, _, _) -> false.

ng2({A,B}) -> A > B.
ng3(A, B) ->  A < B.

n_test_() ->
    [?_assertException(error, function_clause, n(1, b))
    ,?_assertMatch(ok, n(1,2))
    ,?_assertMatch(ok, n(3,2))
    ,{foreach, local,
        fun() -> put(key, value) end,
        [?_assertMatch({error1,value}, n(2,1))
        ,?_assertMatch({error2,value}, n(1,2))]}
].
