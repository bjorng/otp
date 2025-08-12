-module(not_restored).

-export([t/0]).

c(A, Bin) ->
    Res = case A of
              0 ->
                  case Bin of
                      <<L:16,B:L/binary>> ->
                          B;
                      <<_/binary>> ->
                          none
                  end;
              1 ->
                  other
          end,
    id({Res,Bin}).

t() ->
    Bin = id(<<0,3:16,"xyz">>),
    <<_,T/binary>> = Bin,
    Res = {c(0, id(T)), c(1, id(~"abc")), c(0, <<"whatever">>)},
    {{~"xyz",<<0,3,"xyz">>},{other,~"abc"},{none,~"whatever"}} = Res.

id(I) -> I.
