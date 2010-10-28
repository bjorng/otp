
-module(misc).

-compile([export_all, pure_guards]).
-compile([all_pure]).

%% Compile only tests.

g(A, B) ->
    case some:func(A, B) of
        true ->
            B;
        false ->
            A
    end.

s(A,B) when imsets:is_set(A,A) ->
    {set, A};
s(A,B) when imgb_sets:is_set(A) ->
    {gbset, A};
s(A, B) when is_list(A) ->
    {lst, B};
s(A,B) ->
    {none, A}.

nest(A, B) when g1(A) ->
    Res =
    case find(A, B) of
        {ok,Val} when g2(A,B) ->
            [Val|A];
        {ok,Val} when g3(B) ->
            [B|Val];
        error ->
            error
      end,
    [ok|Res].

g1(E) ->
    is_tuple(E).

g2(A, B) ->
    is_float(A) andalso is_list(B).

g3(B) ->
    is_integer(B).

find(A, B) when A > B ->
    {ok, A};
find(_, _) ->
    error.
