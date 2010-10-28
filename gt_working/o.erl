
-module(o).
-compile([export_all, pure_guards]).
%-compile([all_pure]).

-include_lib("eunit/include/eunit.hrl").

a(A, B) when ?MODULE:lst(A) =:= ?MODULE:tup(B) ->
    {ok, B, A};
a(A, B) ->
    {error, A, B}.

b(A, B) when ?MODULE:lst([?MODULE:tup(B)]) -> {ok, B, A};
b(A, B) -> {error, A, B}.

a_test_() ->
    [?_assertMatch({ok,{},[]}, a([], {}))
    ,?_assertMatch({error,[],3}, a([], 3))
    ,?_assertMatch({error,4,{}}, a(4, {}))
    ,?_assertMatch({ok,b,a}, a(a, b)) %% Both tests equal false.
].

b_test_() ->
    [?_assertMatch({ok,{},[]}, b([],{}))].

e(A, B) when is_list(A); ?MODULE:lst(A); ?MODULE:tup(B) ->
  {ok, B, A};
e(A, B) ->
    {error, A, B}.

e_test_() ->
    [?_assertMatch({ok,{},[]}, e([], {}))
    ,?_assertMatch({ok,{},42}, e(42,{}))
    ,?_assertMatch({ok,42,[]}, e([],42))
    ,?_assertMatch({error,a,b}, e(a,b))
].

f(A, B) when is_list(?MODULE:lst(A)); ?MODULE:lst(?MODULE:tup(A)) -> {ok, B, A};
f(A, B) -> {error, A, B}.

f_test_() ->
    [?_assertMatch({error,[],{}}, f([],{}))].

lst(A) -> is_list(A).
tup(A) -> is_tuple(A).
