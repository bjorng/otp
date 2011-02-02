%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(sys_core_mret).

-export([module/2]).

%%%
%%% In this module, we avoid building and returning
%%% a tuple only to have it immediately matched apart by the caller.
%%% Instead of returning a tuple, return several values in {x,0},
%%% {x,1}, and so on.
%%%
%%% We can rewrite a function that returns a tuple to a function
%%% with multiple return values if both of the following conditions
%%% are true:
%%%
%%% (1) The function always return a tuple of the same size, or it
%%%     it tail-recursively calls a function that returns a tuple
%%%     of that same size.
%%%
%%% (2) All callers of the function either match apart the return value,
%%%     or call the function tail-recursively.
%%%

-include("core_parse.hrl").

-import(lists, [foldl/3]).

module(#c_module{defs=Ds0}=Mod, _Opts) ->
    case analyze(Mod) of
	[] ->
	    {ok,Mod};
	RetInfo0 ->
	    io:format("~p\n", [RetInfo0]),
	    RetInfo = gb_trees:from_orddict(lists:sort(RetInfo0)),
	    Ds1 = [function(F, RetInfo) || F <- Ds0],
	    {ok,Mod#c_module{defs=Ds1}}
    end.

function({#c_var{name=FA}=Var,Fun0}, RetInfo) ->
    try
	case gb_trees:lookup(FA, RetInfo) of
	    none ->
		Fun = expr(Fun0, RetInfo, 1),
		{Var,Fun};
	    {value,Values} ->
		Fun1 = expr(Fun0, RetInfo, Values),
		Anno = Fun1#c_fun.anno,
		Fun = Fun1#c_fun{anno=[{return_values,Values}|Anno]},
		{Var,Fun}
	end
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    {F,Arity} = FA,
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

expr(#c_fun{body=B0}=Fun, Ri, Rvals) ->
    B = expr(B0, Ri, Rvals),
    Fun#c_fun{body=B};
expr(#c_case{arg=Arg0,clauses=Cs0}=Case, Ri, Rvals) ->
    case eval_case(Case, Ri) of
	{yes,Let} ->
	    expr(Let, Ri, Rvals);
	no ->
	    Arg = expr(Arg0, Ri, 1),
	    Cs = expr_list(Cs0, Ri, Rvals),
	    Case#c_case{arg=Arg,clauses=Cs}
    end;
expr(#c_clause{body=B0}=C, Ri, Rvals) ->
    B = expr(B0, Ri, Rvals),
    C#c_clause{body=B};
expr(#c_let{arg=Arg0,body=B0}=Let, Ri, Rvals) ->
    Arg = expr(Arg0, Ri, 1),
    B = expr(B0, Ri, Rvals),
    Let#c_let{arg=Arg,body=B};
expr(#c_letrec{defs=Ds0,body=Body0}=Letrec, Ri, Rvals) ->
    Body = expr(Body0, Ri, Rvals),
    Ds = [{FA,expr(Fun, Ri, Rvals)} || {FA,Fun} <- Ds0],
    Letrec#c_letrec{defs=Ds,body=Body};
expr(#c_seq{arg=Arg0,body=Body0}=Seq, Ri, Rvals) ->
    Arg = expr(Arg0, Ri, 1),
    Body = expr(Body0, Ri, Rvals),
    Seq#c_seq{arg=Arg,body=Body};
expr(#c_literal{}=Lit, _Ri, 1) ->
    Lit;
expr(#c_literal{anno=A,val=Tuple}, _Ri, N) when tuple_size(Tuple) =:= N ->
    LitList = [#c_literal{val=E} || E <- tuple_to_list(Tuple)],
    #c_values{anno=A,es=LitList};
expr(#c_binary{}=Bin, _Ri, _Rvals) ->
    Bin;
expr(#c_tuple{es=Es0}=Tuple, Ri, 1) ->
    Es = expr_list(Es0, Ri, 1),
    Tuple#c_tuple{es=Es};
expr(#c_tuple{anno=A,es=Es0}, Ri, N) when length(Es0) =:= N ->
    Es = expr_list(Es0, Ri, 1),
    #c_values{anno=A,es=Es};
expr(#c_cons{hd=H0,tl=T0}=Cons, Ri, _Rvals) ->
    H = expr(H0, Ri, 1),
    T = expr(T0, Ri, 1),
    Cons#c_cons{hd=H,tl=T};
expr(#c_var{}=Var, _Ri, _Rvals) ->
    Var;
expr(#c_primop{args=Args0}=Primop, Ri, _Rvals) ->
    Args = expr_list(Args0, Ri, 1),
    Primop#c_primop{args=Args};
expr(#c_call{args=Args0}=Call, Ri, _Rvals) ->
    Args = expr_list(Args0, Ri, 1),
    Call#c_call{args=Args};
expr(#c_apply{args=Args0}=Apply, Ri, _Rvals) ->
    Args = expr_list(Args0, Ri, 1),
    Apply#c_apply{args=Args};
expr(#c_try{arg=A0,vars=[#c_var{name=V}],body=#c_var{name=V},
	    handler=H0}=Try, Ri, Rvals) ->
    A = expr(A0, Ri, Rvals),
    Vars = new_vars(Rvals),
    B = #c_values{es=Vars},
    H = expr(H0, Ri, Rvals),
    Try#c_try{arg=A,vars=Vars,body=B,handler=H};
expr(#c_try{arg=A0,body=B0,handler=H0}=Try, Ri, _) ->
    A = expr(A0, Ri, 1),
    B = expr(B0, Ri, 1),
    H = expr(H0, Ri, 1),
    Try#c_try{arg=A,body=B,handler=H};
expr(#c_catch{body=B0}=Catch, Ri, _Rvals) ->
    B = expr(B0, Ri, 1),
    Catch#c_catch{body=B};
expr(#c_values{es=Es0}=Val, Ri, _Rvals) ->
    Es = expr_list(Es0, Ri, 1),
    Val#c_values{es=Es};
expr(#c_receive{clauses=Cs0,action=B0}=Recv, Ri, Rvals) ->
    Cs = expr_list(Cs0, Ri, Rvals),
    B = expr(B0, Ri, Rvals),
    Recv#c_receive{clauses=Cs,action=B}.

expr_list(Es, Ri, Rvals) ->
    [expr(E, Ri, Rvals) || E <- Es].

eval_case(#c_case{arg=#c_apply{op=#c_var{name={_,_}=FA}}}=Case, Ri) ->
    case gb_trees:lookup(FA, Ri) of
	{value,Values} ->
	    {yes,eval_case_1(Values, Case)};
	none ->
	    no
    end;
eval_case(_, _) -> no.

eval_case_1(Rvals, #c_case{arg=Arg,clauses=Cs0}=Case0) ->
    Vars = new_vars(Rvals),
    Cs = rewrite_clauses(Cs0, Vars, Rvals),
    Case = Case0#c_case{arg=#c_values{es=Vars},clauses=Cs},
    #c_let{vars=Vars,arg=Arg,body=Case}.

rewrite_clauses([#c_clause{pats=Pats,body=Body}=C0|Cs], Vars, Rvals) ->
    case Pats of
	[#c_tuple{es=Es}] when length(Es) =:= Rvals ->
	    C = C0#c_clause{pats=Es},
	    [C|rewrite_clauses(Cs, Vars, Rvals)];
	[#c_literal{val=Tuple}] when tuple_size(Tuple) =:= Rvals ->
	    Values = [#c_literal{val=E} || E <- tuple_to_list(Tuple)],
	    C = C0#c_clause{pats=Values},
	    [C|rewrite_clauses(Cs, Vars, Rvals)];
	[#c_var{}=Var] ->
	    Let = #c_let{vars=[Var],arg=#c_tuple{es=Vars},body=Body},
	    C = C0#c_clause{pats=Vars,body=Let},
	    [C|rewrite_clauses(Cs, Vars, Rvals)];
	_Other ->
	    %% XXX Add warning.
	    rewrite_clauses(Cs, Vars, Rvals)
    end;
rewrite_clauses([], _, _) -> [].

new_vars(N) ->
    new_vars_1(N, []).

new_vars_1(0, Acc) ->
    Acc;
new_vars_1(N, Acc) ->
    V = #c_var{name=list_to_atom("mret"++integer_to_list(N))},
    new_vars_1(N-1, [V|Acc]).

%% analyze(Module) -> [{{Name,Arity},NumReturnValues}]
%%  Analyze a module to find all functions that can be rewritten to
%%  have multiple return values. The returned list will only
%%  contain the functions that can be rewritten (i.e. NumReturnValues
%%  will be greater than or equal to 2).

analyze(#c_module{defs=Ds0,exports=Exp0}) ->
    %% We'll need a digraph to keep track of tail-recursive calls,
    %% so that we can propagate information in both directions any
    %% number of levels.
    G = digraph:new([private]),

    %% Add a digraph vertex for each function. The label starts
    %% out as 'unknown', meaning that we don't know yet whether
    %% it can be rewritten to have multiple return values.
    [digraph:add_vertex(G, FA, unknown) || {#c_var{name=FA},_} <- Ds0],

    %% Since we cannot know anything about the callers of the
    %% exported functions, we must force all exported functions
    %% to a 'single' return value.
    [digraph:add_vertex(G, FA, single) || #c_var{name=FA} <- Exp0],

    %% Create edges for all tail-recursive calls. Also mark
    %% vertices as we go.
    graph(G, Ds0),

    %% Now propagate the information along the edges (both up and
    %% down) until nothing more changes.
    iter(G),

    %% Retrieve vertex names and labels and filter away the functions
    %% that may only return a single value.
    Vs0 = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    Vs = [V || {_,Rvals}=V <- Vs0, Rvals =/= single],
    digraph:delete(G),

    Vs.

graph(G, [{#c_var{name=Name},Fun}|T]) ->
    graph(G, Fun, Name, true),
    graph(G, T);
graph(_, []) -> ok.

graph(G, #c_fun{body=B}, Name, Top) ->
    graph(G, B, Name, Top);
graph(G, #c_case{arg=#c_apply{args=Args,op=#c_var{name={_,_}=FA}},
		   clauses=Cs}, Name, Top) ->
    %% case local_function(...) of ... end
    %%
    %% Unless there are some complications in the matching,
    %% the called function is allowed to return multiple values.
    graph_list(G, Args, Name, false),
    graph_list(G, Cs, Name, Top),
    case complicated_matching(Cs) of
	false -> ok;
	true -> meet(G, FA, single)
    end;
graph(G, #c_case{arg=Arg,clauses=Cs}, Name, Top) ->
    graph(G, Arg, Name, false),
    graph_list(G, Cs, Name, Top);
graph(G, #c_apply{args=Args,op=#c_var{name={_,_}=Callee}}, Name, Top) ->
    graph_list(G, Args, Name, false),
    case Top of
	true ->
	    %% Tail-recursive call. Add an edge from the caller to
	    %% the callee.
	    add_edge(G, Name, Callee),

	    %% This not strictly necessary, but may save some iterations later.
	    meet(G, Callee, get_rvals(G, Name));
	false ->
	    %% Body-recursive call, not protected by a 'case'.
	    %% We cannot allow Callee to return more than one value.
	    meet(G, Callee, single)
    end;
graph(G, #c_apply{args=Args}, Name, Top) ->
    graph_list(G, Args, Name, false),
    meet(G, Name, single, Top);
graph(G, #c_call{args=Args}, Name, Top) ->
    graph_list(G, Args, Name, false),
    meet(G, Name, single, Top);
graph(G, #c_clause{body=B}, Name, Top) ->
    graph(G, B, Name, Top);
graph(G, #c_let{arg=Arg,body=B}, Name, Top) ->
    graph(G, Arg, Name, false),
    graph(G, B, Name, Top);
graph(G, #c_letrec{defs=Defs,body=B}, Name, Top) ->
    meet(G, Name, single, Top),
    [digraph:add_vertex(G, FA) || {#c_var{name=FA},_} <- Defs],
    Funs = [Fun || {_,Fun} <- Defs],
    graph_list(G, Funs, Name, Top),
    graph(G, B, Name, Top);
graph(G, #c_seq{arg=Arg,body=B}, Name, Top) ->
    graph(G, Arg, Name, false),
    graph(G, B, Name, Top);
graph(G, #c_var{name=FA}, Name, Top) ->
    case FA of
	{_,_} ->
	    %% fun F/A. Since we don't know from where the fun may
	    %% be called, FA cannot be allowed to return multiple values.
	    meet(G, FA, single);
	_ -> ok
    end,
    case Top of
	false -> ok;
	true -> meet(G, Name, single)
    end;
graph(G, #c_literal{val=Tuple}, Name, true) when is_tuple(Tuple) ->
    %% A literal tuple is returned.
    case tuple_size(Tuple) of
	Sz when 2 =< Sz, Sz =< 255 ->
	    meet(G, Name, Sz);
	_ ->
	    meet(G, Name, single)
    end;
graph(G, #c_literal{}, Name, Top) ->
    %% Non-tuple literal.
    case Top of
	false -> ok;
	true -> meet(G, Name, single)
    end;
graph(G, #c_tuple{es=Es}, Name, true) ->
    %% A tuple is returned.
    graph_list(G, Es, Name, false),
    case length(Es) of
	Sz when 2 =< Sz, Sz =< 255 ->
	    meet(G, Name, Sz);
	_ ->
	    meet(G, Name, single)
    end;
graph(G, #c_tuple{es=Es}, Name, _Top) ->
    graph_list(G, Es, Name, false);
graph(G, #c_binary{}, Name, Top) ->
    meet(G, Name, single, Top);
graph(G, #c_values{es=Es}, Name, _Top) ->
    graph_list(G, Es, Name, false);
graph(G, #c_cons{hd=Hd,tl=Tl}, Name, Top) ->
    graph(G, Hd, Name, false),
    graph(G, Tl, Name, false),
    meet(G, Name, single, Top);
graph(G, #c_primop{args=Args}, Name, _Top) ->
    graph_list(G, Args, Name, false);
graph(G, #c_catch{body=B}, Name, Top) ->
    meet(G, Name, single, Top),
    graph(G, B, Name, false);
graph(G, #c_try{arg=A,vars=[#c_var{name=V}],
		body=#c_var{name=V},handler=H}, Name, Top) ->
    graph(G, A, Name, Top),
    graph(G, H, Name, Top);
graph(G, #c_try{arg=A,body=B,handler=H}, Name, Top) ->
    meet(G, Name, single, Top),
    graph(G, A, Name, false),
    graph(G, B, Name, false),
    graph(G, H, Name, false);
graph(G, #c_receive{clauses=Cs,action=Body}, Name, Top) ->
    graph_list(G, Cs, Name, Top),
    graph(G, Body, Name, Top).

graph_list(G, [H|T], Name, Top) ->
    graph(G, H, Name, Top),
    graph_list(G, T, Name, Top);
graph_list(_, [], _, _) -> ok.

complicated_matching([#c_clause{pats=[#c_alias{}]}|_]) ->
    true;
complicated_matching([#c_clause{guard=G}|Cs]) ->
    case G of
	#c_literal{} -> complicated_matching(Cs);
	_ -> true
    end;
complicated_matching([_|Cs]) ->
    complicated_matching(Cs);
complicated_matching([]) -> false.

iter(G) ->
    Vs = digraph_utils:postorder(G),
    iter(G, Vs).

iter(G, Vs) ->
    AnyChange = foldl(fun(V, Ch) ->
			      iter_vertex(G, V, Ch)
		      end, false, Vs),
    if
	AnyChange ->
	    iter(G, Vs);
	true ->
	    ok
    end.

iter_vertex(G, V, Ch0) ->
    %% First calculate the meet function for all callers
    %% of this function.
    InEdges = digraph:in_edges(G, V),
    Rvals = get_rvals(G, V),
    Ch = foldl(fun({Caller,_}, C0) ->
		       %% Propagate upwards towards the caller.
		       C = C0 or meet(G, Caller, Rvals),

		       %% Propagate downwards to this function.
		       CallersRvals = get_rvals(G, Caller),
		       C or meet(G, V, CallersRvals)
	       end, Ch0, InEdges),
    Ch.

add_edge(_, V, V) ->
    ok;
add_edge(G, V1, V2) ->
    E = {V1,V2},
    E = digraph:add_edge(G, E, V1, V2, []).

%% meet(Graph, VertexName, Value, Top) -> true|false.
%%  Call meet/3 only if Top is 'true', i.e. this core-erlang object
%%  represents the return value for the function being analyzed.
%%
meet(G, Name, Rvals, true) ->
    meet(G, Name, Rvals);
meet(_, _, _, false) -> ok.

%% meet(Graph, VertexName, Value) -> true|false.
%%  Calculate the meet function between the vertex label for
%%  VertexName and Value and assign the value to the label.
%%  Return 'true' if the label was actually changed, and
%%  'false' otherwise.
%%
%%  The meet function is calculated by starting with
%%  the values given and follow the lines down until they meet
%%  in the following lattice:
%%
%%               unknown
%%                 /\
%%		  /  \
%%		 /    \
%%		/      \
%%	       2 ..... 255
%%	       	\      	/
%%		 \     /
%%		  \   /
%%		   \ /
%%	         single
%%
%%  Examples: meet(2, 2) => 2
%%            meet(unknown, 3) => 3
%%            meet(2, 3) => single
%%            meet(3, single) => single
%%
meet(G, Name, Rvals) ->
    case get_rvals(G, Name) of
	Rvals ->
	    %% Same value as before.
	    false;
	single ->
	    %% Already 'single' -- no change.
	    false;
	unknown ->
	    %% Change from no information.
	    digraph:add_vertex(G, Name, Rvals),
	    true;
	_ ->
	    %% Inconsistent number of return values.
	    digraph:add_vertex(G, Name, single),
	    true
    end.

%% get_rvals(Graph, VertexName) -> unknown | integer() | single.
%%  Return the label for VertexName in the digraph.
%%
get_rvals(G, Name) ->
    {Name,Label} = digraph:vertex(G, Name),
    Label.
