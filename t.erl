-module(t).
-compile([export_all,nowarn_export_all]).

efficient({Var}) ->
    catch
	receive _ ->
		Var
	end.

r() ->
    R = receive
            {x,Msg} ->
                Msg+4;
            Any ->
                {any,Any}
        end,
    {ok,R}.

r(Mref, Process, Timeout) ->
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, noconnection} ->
            Node = ?MODULE:get_node(Process),
            exit({nodedown, Node});
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
    after Timeout ->
            erlang:demonitor(Mref, [flush]),
            exit(timeout)
    end.

%% burns(Richmond) when Richmond#{true := 0}; a ->
%%     specification.

%% csemi7(A, B, C) when A#{a:=B} > #{a=>1}; abs(C) > 2 -> ok;
%% csemi7(_, _, _) -> error.

try_catch(E) ->
    {ok,catch E()}.

try_try(E) ->
    try
        E()
    catch
        throw:Thrown:Stk ->
            {Thrown,Stk};
        Class:Reason:Stk ->
            erlang:raise(Class, Reason, Stk)
    end.

try_try_notail(E) ->
    try
        E()
    catch
        throw:Thrown:Stk ->
            {Thrown,Stk};
        Class:Reason:Stk ->
            erlang:raise(Class, Reason, Stk)
    end,
    ok.

map(#{x:=X,y:=_}) ->
    X.

dump(Fs, TS) ->
    [F#{dts => DTS / TS} || #{dts := DTS} = F <- Fs].

append_bin(Bin, X) ->
    <<Bin/binary,X:32>>.

bc(L) ->
    << <<W:32>> || W <- L >>.

build_bin(A, Unicode, Bin, Tail) ->
    <<A:16,"abc",Unicode/utf8,Bin/bits,Tail/binary>>.

escape_char(Char) when Char < 256 ->
    <<A:4,B:4>> = <<Char:8>>,
    ["\\x",to_hex(A),to_hex(B)].

-compile({inline, [to_hex/1]}).
to_hex(C) when is_integer(C) andalso C >= 0 andalso C =< 9 -> $0 + C;
to_hex(C) when is_integer(C) andalso C >= 10 andalso C =< 15 -> $A + C - 10.

break(X) ->
    Res = case X of
              a ->
                  V = X+10,
                  1;
              b ->
                  V = X+20,
                  2
              %% _ ->
              %%     V = other,
              %%     9999
          end,
    {V,Res + 99}.

match_string(<<"abc",Int:16,0:0,_/binary>>) ->
    Int.

bin_to_list(<<H1,H2,T/binary>>) ->
    [H1,H2|bin_to_list(T)];
bin_to_list(<<H,T/binary>>) ->
    [H|bin_to_list(T)];
bin_to_list(<<>>) ->
    [].

bin(<<H,T/binary>> = Bin) ->
    {Bin,H,T}.

put_map1(M1, M2) ->
    if
        M1#{b:=xyz} =:= M2 ->
            ok;
        true ->
            error
    end.

put_map2(K, A, B, C) ->
    Map0 = #{a=>A,b=>B,c=>C},
    Map1 = Map0#{K=>42},
    Map2 = Map1#{a:=A+99},
    Map2.


-record(r, {a,b,c,d,e,f,g}).

dsetelement(#r{}=R) ->
    R#r{a=x,b=y}.

select_lit({a,b}) ->
    tuple;
select_lit(a) ->
    list;
select_lit([H|T]) ->
    {H,T}.

from_term(T) ->
    Type = case T of
               _ when is_list(T) -> [any];
               _ -> any
           end,
    try ?MODULE:setify(T, Type)
    catch _:_ -> erlang:error(badarg)
    end.

try_not(A) ->
    A.

foldit([H|T], A) when H rem 2 =:= 0 ->
    foldit(T, A+1);
foldit([_|T], A) ->
    foldit(T, A-1);
foldit([], A) ->
    A.

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) when is_function(F, 1) -> ok.

find_beam(Module) when is_atom(Module) ->
    case code:which(Module) of
        Other when Other =:= ""; Other =:= cover_compiled ->
            ?MODULE:find_beam_1(Module);
        Error ->
            Error
    end.

t({ok,A}) ->
    A;
t({ok,A,B}) ->
    A+B;
t(error) ->
    error.

seq_loop(N, X, L) when N >= 4 ->
     seq_loop(N-4, X-4, [X-3,X-2,X-1,X|L]);
seq_loop(N, X, L) when N >= 2 ->
     seq_loop(N-2, X-2, [X-1,X|L]);
seq_loop(1, X, L) ->
     [X|L];
seq_loop(0, _, L) ->
     L.

concat(List) ->
    lists:flatmap(fun thing_to_list/1, List).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string

unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {lists:reverse(Xs), lists:reverse(Ys)}.

zip([X | Xs], [Y | Ys]) -> [{X, Y} | zip(Xs, Ys)];
zip([], []) -> [].

setelement(_Config) ->
    T0 = id({a,42}),
    {a,_} = T0,
    {b,_} = setelement(1, T0, b),
    ok.

id(I) -> I.
