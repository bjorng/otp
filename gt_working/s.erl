
-module(s).
-compile([export_all, pure_guards]).
%-compile([all_pure]).

-include_lib("eunit/include/eunit.hrl").

%%% Simple tests.

lst(A) -> is_list(A).
tup(A) -> is_tuple(A).
ato(A) -> is_atom(A).

%% Test code generation for one uguard.
a(A) when lst(A) -> {lst,A};
a(A) -> {error,A}.

a_test_() ->
    [?_assertMatch({lst,[]}, a([]))
    ,?_assertMatch({error,a}, a(a))
].

%% Two cases, one bif guard.
b(A) when lst(A) -> {lst, A};
b(A) when is_integer(A) -> {int, A};
b(A) -> {error,A}.

b_test_() ->
    [?_assertMatch({lst,[]}, b([]))
    ,?_assertMatch({int,42}, b(42))
    ,?_assertMatch({error,a}, b(a))
].

%% Two cases, one bif guard, but leading.
c(A) when is_integer(A) -> {int, A};
c(A) when lst(A) -> {lst, A};
c(A) -> {error,A}.

c_test_() ->
    [?_assertMatch({lst,[]}, c([]))
    ,?_assertMatch({int,42}, c(42))
    ,?_assertMatch({error,a}, c(a))
].


%% Three or more guards, it should be safe to assume any more work.

f(A) when lst(A) -> {lst, A};
f(A) when is_integer(A) -> {int, A};
f(A) when tup(A) ->  {tup, A};
f(A) when ato(A) -> {ato,A};
f(A) -> {error, A}.

f_test_() ->
    [?_assertMatch({lst,[]}, f([]))
    ,?_assertMatch({int,42}, f(42))
    ,?_assertMatch({tup,{a}}, f({a}))
    ,?_assertMatch({ato,a}, f(a))
    ,?_assertMatch({error,3.3}, f(3.3))
].

%% Check match failure as well.

g(A) when lst(A) -> {lst, A};
g(A) when is_integer(A) -> {int, A};
g(A) when tup(A) ->  {tup, A}.

g_test_() ->
    [?_assertMatch({lst,[]}, g([]))
    ,?_assertMatch({int,42}, g(42))
    ,?_assertMatch({tup,{a}}, g({a}))
    ,?_assertException(error, function_clause, g(3.3))
].
