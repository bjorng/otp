-module(gh_7478_b).
-export([?MODULE/0, gh_7478_b/1]).

?MODULE() ->
    <<>> = gh_7478_b([]),
    <<>> = gh_7478_b([1]),
    ok = try gh_7478_b(<<>>) catch _:_ -> ok end,
    ok = try gh_7478_b(a) catch _:_ -> ok end,
    ok.

gh_7478_b(L) ->
    <<0 || erlang:yield() and ([0 || _ <- L] =< 0),
           <<>> <= ok >>.
