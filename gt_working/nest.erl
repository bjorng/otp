
-module(nest).
-compile([export_all,pure_guards]).
%-compile([all_pure]).

-include_lib("eunit/include/eunit.hrl").

%% Nested matches.

n(A, B) when g1(A, B) ->
    case proc(A, B) of
        Val when g2(Val) ->
            {v1, {Val, A, B}};
        {V1,V2} when g3(V1, V2) ->
            case proc(V1,V2) of
                D when g2(D) ->
                    {v3,D,V1};
                D ->
                    {v2, {B, V2, A, V1,D}}
            end
    end;
n(A, B) when g4(A,B) ->
    {f2, [A|B]}.

proc(A, B) -> {A*B, A+B}.

g1(A, B) -> is_integer(A) andalso is_integer(B).
g2({A, B}) -> A > B.
g3(A, B) -> A < B.
g4(A, B) -> is_atom(A) andalso is_atom(B).

n_test_() ->
    [?_assertMatch({v1,{{6,5},2,3}}, n(2,3))
    ,?_assertMatch({v3,{20,9},4}, n(1,4))
    ,?_assertMatch({v2,{1,2,1,1,{2,3}}}, n(1,1))
    ,?_assertException(error, {case_clause,{4,4}}, n(2,2))
    ,?_assertMatch({f2,[a|b]}, n(a,b))
    ,?_assertException(error, function_clause, n({a},a))
].
