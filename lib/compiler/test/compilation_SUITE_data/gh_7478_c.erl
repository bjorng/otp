-module(gh_7478_c).
-export([?MODULE/0, gh_7478_c/1]).

?MODULE() ->
    ok = try gh_7478_c(false) catch error:{bad_generator,ok} -> ok end,
    ok = try gh_7478_c(true) catch error:{bad_generator,ok} -> ok end,
    ok = try gh_7478_c([]) catch error:{bad_generator,ok} -> ok end,
    ok.

gh_7478_c(A) ->
    <<0 || try
               [ 0 || _ := _ <- ok]
           catch
               _ ->
                   false
           end and A,
           _ <- ok
    >>.
