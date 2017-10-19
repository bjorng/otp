-module(dgud_length).

-compile([export_all, nowarn_export_all]).
-define(ASCII_LIST(CP1,CP2), CP1 < 256, CP2 < 256, CP1 =/= $\r).

gc([CP1|[CP2|_]=T])
  when CP1 < 256, CP2 < 256, CP1 =/= $\r -> %% Ascii Fast path
    [CP1|T];
gc(<<CP1/utf8, Rest/binary>>) ->
    if CP1 < 256, CP1 =/= $ ->
           case Rest of
               <<CP2/utf8, _/binary>> when CP2 < 256 -> %% Ascii Fast path
                   [CP1|Rest];
               _ ->
                   gc_1([CP1|Rest])
           end;
      true ->
            gc_1([CP1|Rest])
    end;
gc(Str) ->
    gc_1(Str).

%% Possible syntax extension:
%%
%% gc(<<CP1/utf8, <<CP2/utf8,  _/binary>> = Rest >>) ->
%%     if CP1 < 256, CP2 < 256, CP1 =/= $\r -> %% Ascii Fast path
%%             [CP1|Rest];
%%       true -> gc_1([CP1|Rest])
%%     end;


gc_1(Str) ->
    {ok, Str}.

%% Opt of length

length(CD) when is_binary(CD) ->
    %%simple(CD, 0),
    %%simple_build(CD, 0),
    %%simple_if(CD,0),
    case unicode_util:cp(CD) of
        [] -> 0;
        [Cp|Bin] ->
            pick_one(Cp, Bin, 0)
            %% pick_one_guard(Cp, Bin, 0)
    end.

pick_one(CP1, <<CP2/utf8, Rest/binary>>=Bin0, N) ->
    if ?ASCII_LIST(CP1,CP2) ->
            pick_one(CP2, Rest, N+1);
       true ->
            [_|Bin1] = unicode_util:gc([CP1|Bin0]),
            case unicode_util:cp(Bin1) of
                [] -> N+1;
                [CP3|Bin] -> pick_one(CP3, Bin, N+1)
            end
    end;
pick_one(_, <<>>, N) -> N+1.

pick_one_guard(CP1, <<CP2/utf8, Rest/binary>>, N) when ?ASCII_LIST(CP1,CP2) ->
    pick_one_guard(CP2, Rest, N+1);
pick_one_guard(CP1, Bin0, N) ->
    [_|Bin1] = unicode_util:gc([CP1|Bin0]),
    case unicode_util:cp(Bin1) of
        [] -> N+1;
        [CP3|Bin] -> pick_one_guard(CP3, Bin, N+1)
    end.

%% This one can be optimized.
pick_one_guard_alt(<<CP2/utf8, Rest/binary>>, CP1, N) when ?ASCII_LIST(CP1,CP2) ->
    pick_one_guard_alt(Rest, CP2, N+1);
pick_one_guard_alt(Bin0, CP1, N) ->
    [_|Bin1] = unicode_util:gc([CP1|Bin0]),
    case unicode_util:cp(Bin1) of
        [] -> N+1;
        [CP3|Bin] -> pick_one_guard_alt(Bin, CP3, N+1)
    end.

simple(<<CP1/utf8, Rest/binary>> = Bin0, N) ->
    case Rest of
        <<CP2/utf8, _/binary>> when ?ASCII_LIST(CP1,CP2) ->
            simple(Rest, N+1);
        _ ->
            [_|Bin1] = unicode_util:gc(Bin0),
            simple(Bin1, N+1)
    end;
simple(<<>>, N) -> N.

simple_build(<<CP1/utf8, Rest/binary>>, N) ->
    case Rest of
        <<CP2/utf8, _/binary>> when ?ASCII_LIST(CP1,CP2) ->
            simple_build(Rest, N+1);
        _ ->
            [_|Bin1] = unicode_util:gc([CP1|Rest]),
            simple_build(Bin1, N+1)
    end;
simple_build(<<>>, N) -> N.

simple_if(<<CP1/utf8, Rest/binary>>, N) ->
    if CP1 < 255, CP1 =/= $\r ->
            case Rest of
                <<CP2/utf8, _/binary>> when CP2 < 255 ->
                    simple_if(Rest, N+1);
                _ ->
                    [_|Bin1] = unicode_util:gc([CP1|Rest]),
                    simple_if(Bin1, N+1)
            end;
       true ->
            [_|Bin1] = unicode_util:gc([CP1|Rest]),
            simple_if(Bin1, N+1)
    end;
simple_if(<<>>, N) -> N.
