
-module(mult).
-compile([export_all, pure_guards]).
%-compile([all_pure]).

%% Multiple guard statements.

%% This actually works, it's not until bif guards precede user
%% guards that problems arise.
a(A, B) when ?MODULE:lst(A) orelse ?MODULE:tup(B) ->
    {ok, B, A};
a(A, B) ->
    {error, A, B}.

b(A, B) when is_list(A) orelse is_tuple(B) orelse ?MODULE:lst(B) ->
    {ok, B, A};
b(A, B) ->
    {error, A, B}.

c(A, B) when is_list(A) andalso is_tuple(B) andalso ?MODULE:lst(B) ->
    {ok, B, A};
c(A, B) ->
    {error, A, B}.

f(A, B) ->
    case A of
        D when length(D) =:= 2 ->
            case B of
                E when is_list(E) andalso is_list(B) andalso length(E) =:= 3 ->
                    ok;
                _ -> error1
            end;
        _ -> error2
    end.

lst(A) -> is_list(A).
tup(A) -> is_tuple(A).
