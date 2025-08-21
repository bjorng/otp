-module(wfbm).
-export([wfbm/0,id/1]).
-compile([export_all, nowarn_export_all]).

-define(DATELEN, 16).

check_for_dot_or_space(Bin) ->
    check_for_dot_or_space(Bin, 0).

check_for_dot_or_space(<<$\s, _/binary>>, 0) ->
    {nomatch,0};
%% check_for_dot_or_space(Bin, 10) ->
%%     io:format("~w\n", [Bin]),
%%     {blurf,Bin};
check_for_dot_or_space(Bin, Len) ->
    %% io:format("~w ~w\n", [Bin,Len]),
    case Bin of
        <<Front:Len/binary, $\s, _/binary>> ->
            {ok,Front};
        <<_:Len/binary, $., _/binary>> ->
            {nomatch,Len};
        _ ->
            %% io:format("~w\n", [Bin]),
            check_for_dot_or_space(Bin, Len+1)
    end.

get_tail(<<>>) ->
    nomatch;
get_tail(Bin) ->
    <<Front:?DATELEN/binary, Tail/binary>> = Bin,
    case Front of
        <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
            case check_for_dot_or_space(Tail) of
                {ok,Match} ->
                    {ok,<<Y/binary,$/,M/binary,$/,D/binary,$/, Match/binary>>}
                %% {nomatch,Skip} -> {skip,?DATELEN + Skip}
            end;
        _ -> nomatch
    end.

wfbm() ->
    {nomatch,1} = check_for_dot_or_space(<<"g.urka">>),
    {ok,<<"2007/10/23/blurf">>} = get_tail(<<"200x/2007/10/23/blurf ">>),
    ok.

id(I) ->
    I.
 
