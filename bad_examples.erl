-module(bad_examples).
-compile([export_all,nowarn_export_all]).

abc(<<Def:Sz>>, #{Def := Sz}) ->
    ok.

bazzier(#{{key,K1} := <<Result:32>>, {key,K2} := {bin,<<K1:Sz>>}}, K2) ->
    Result.

chained(#{K1 := <<Result:32>>, K2 := {bin,K1}, K3 := [K2], K4 := #{key := K4}}, K4) ->
    Result.
