-module(gh_7478_a).
-export([?MODULE/0, gh_7478_a/1]).

?MODULE() ->
    ok = try gh_7478_a(0) catch _:_ -> ok end,
    ok = try gh_7478_a([]) catch _:_ -> ok end,
    ok = try gh_7478_a(<<>>) catch _:_ -> ok end,
    ok = try gh_7478_a(a) catch _:_ -> ok end,
    ok.

gh_7478_a(A) ->
    [ 0 ||
        begin
            _ = bit_size(maybe
                             [] ?= maybe
                                       0 ?= A,
                                       << 0 || _ <- []>>,
                                       ok
                                   end,
                             A
                         else
                             A -> A;
                             ok -> A
                         end),
            A end,
        _ <- ok].
