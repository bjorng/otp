-module(gh_11415).
-export([gh_11415/0, main/1]).

f(_, P) ->
    {_, S} = d(P - 1, 0),
    if
        S =:= 0 ->
           g(0,0,0,0,0)
    end.

g(_, _, _, _, _) ->
    i(0, 0, 0).

d(Q, S) when Q rem 2 =:= 0 ->
    d(Q div 2, S + 1);
d(Q, S) ->
    {Q, bnot S}.

i(_, _, _) ->
    i(0,0,0,0).

i(Base, Exp, Mod, _) ->
    _ = case Exp rem 2 of
        1 -> 1;
        0 -> 0
        end,
    i(Base, Exp div 2, Mod, 0).

main(_) ->
    f(0, 1003).

gh_11415() ->
    ok.
