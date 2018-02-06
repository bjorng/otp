%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose: Convert the Kernel Erlang format to the SSA format.

-module(beam_kernel_to_ssa).

%% The main interface.
-export([module/2]).

-import(lists, [member/2,keymember/3,keysort/2,
		append/1,flatmap/2,foldl/3,foldr/3,mapfoldl/3,
		sort/1,reverse/1,reverse/2,map/2]).
-import(ordsets, [add_element/2,intersection/2,union/2]).

-include("v3_kernel.hrl").
-include("beam_ssa.hrl").

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).

%% Main codegen structure.
-record(cg, {lcount=1,              %Label counter
             vars=#{} :: map(),     %Defined variables.
	     bfail,                 %Fail label for BIFs
	     break,                 %Break label
	     recv,                  %Receive label
	     in_catch=false,        %Inside a catch or not.
	     ultimate_failure,      %Label for ultimate match failure.
             badarg_failure,        %Label for badarg failure.
             ctx                    %Match context.
	    }).

%% Stack/register state record.
%% FIXME: To be eliminated.
-record(sr, {reg=[],				%Register table
	     stk=[],				%Stack table
	     res=[]}).				%Registers to reserve

%% Internal records.
-record(cg_block, {anno=[] :: term(),
                   es=[] :: [term()]}).
-record(cg_break, {args,phi}).
-record(cg_phi, {vars}).
-record(cg_unreachable, {vars}).

-type vdb_entry() :: {atom(),non_neg_integer(),non_neg_integer()}.

%% FIXME: To be eliminated.
-record(l, {i=0 :: non_neg_integer(),           %Op number
	    vdb=[] :: [vdb_entry()],            %Variable database
	    a=[] :: [term()]}).                 %Core annotation

-spec module(#k_mdef{}, [compile:option()]) -> {'ok',#b_module{}}.

module(#k_mdef{name=Mod,exports=Es,attributes=Attr,body=Forms}, _Opts) ->
    Body = functions(Forms, Mod),
    Module = #b_module{name=Mod,exports=Es,attributes=Attr,body=Body},
    {ok,Module}.

functions(Forms, Mod) ->
    [function(F, Mod) || F <- Forms].

function(#k_fdef{anno=#k{a=Anno},func=Name,arity=Arity,
                 vars=As0,body=Kb}, Mod) ->
    try
        #k_match{} = Kb,                   %Assertion.

        %% Annotate kernel records with variable usage.
        %% FIXME: To be ripped out. But note that this
        %% pass translates #k_seq{} to #cg_block{}.
        %% That must be handled in another place.
        Vdb0 = init_vars(As0),
        {Body,_,Vdb} = body(Kb, 1, Vdb0),

        %% Generate the SSA form immediate format.
        St0 = #cg{},
        {As,St1} = init_ssa_args(As0, St0),
        {Asm,St} = cg_fun(Body, As0, Vdb, St1),
        FuncInfo = [{func_info,{{Mod,Name,Arity},line_anno(Anno)}}],
        #b_func{anno=FuncInfo,args=As,bs=Asm,cnt=St#cg.lcount}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

%% FIXME: To be eliminated.
%%
%% This pass creates beam format annotated with variable lifetime
%% information.  Each thing is given an index and for each variable we
%% store the first and last index for its occurrence.  The variable
%% database, VDB, attached to each thing is only relevant internally
%% for that thing.
%%
%% For nested things like matches the numbering continues locally and
%% the VDB for that thing refers to the variable usage within that
%% thing.  Variables which live through a such a thing are internally
%% given a very large last index.  Internally the indexes continue
%% after the index of that thing.  This creates no problems as the
%% internal variable info never escapes and externally we only see
%% variable which are alive both before or after.
%%
%% This means that variables never "escape" from a thing and the only
%% way to get values from a thing is to "return" them, with 'break' or
%% 'return'.  Externally these values become the return values of the
%% thing.  This is no real limitation as most nested things have
%% multiple threads so working out a common best variable usage is
%% difficult.

%% body(Kbody, I, Vdb) -> {[Expr],MaxI,Vdb}.
%%  Handle a body.

body(#k_seq{arg=Ke,body=Kb}, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    {Es,MaxI,Vdb2} = body(Kb, I+1, Vdb1),
    E = expr(Ke, I, Vdb2),
    {[E|Es],MaxI,Vdb2};
body(Ke, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    E = expr(Ke, I, Vdb1),
    {[E],I,Vdb1}.

%% expr(Kexpr, I, Vdb) -> Expr.

expr(#k_test{anno=A}=Test, I, _Vdb) ->
    Test#k_test{anno=#l{i=I,a=A#k.a}};
expr(#k_call{anno=A}=Call, I, _Vdb) ->
    Call#k_call{anno=#l{i=I,a=A#k.a}};
expr(#k_enter{anno=A}=Enter, I, _Vdb) ->
    Enter#k_enter{anno=#l{i=I,a=A#k.a}};
expr(#k_bif{anno=A}=Bif, I, _Vdb) ->
    Bif#k_bif{anno=#l{i=I,a=A#k.a}};
expr(#k_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    L = #l{i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a},
    #k_match{anno=L,body=M,ret=Rs};
expr(#k_guard_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    L = #l{i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a},
    #k_guard_match{anno=L,body=M,ret=Rs};
expr(#k_protected{}=Protected, I, Vdb) ->
    protected(Protected, I, Vdb);
expr(#k_try{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh}=Try, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, locked, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    L = #l{i=I,vdb=Tdb1,a=A#k.a},
    Try#k_try{anno=L,
              arg=#cg_block{es=Aes,anno=#l{i=I+1,vdb=Adb,a=[]}},
              vars=Vs,body=#cg_block{es=Bes,anno=#l{i=I+3,vdb=Bdb,a=[]}},
              evars=Evs,handler=#cg_block{es=Hes,anno=#l{i=I+3,vdb=Hdb,a=[]}}};
expr(#k_try_enter{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, 1000000, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    L = #l{i=I,vdb=Tdb1,a=A#k.a},
    #k_try_enter{anno=L,
                 arg=#cg_block{es=Aes,anno=#l{i=I+1,vdb=Adb,a=[]}},
                 vars=Vs,body=#cg_block{es=Bes,anno=#l{i=I+3,vdb=Bdb,a=[]}},
                 evars=Evs,handler=#cg_block{es=Hes,anno=#l{i=I+3,vdb=Hdb,a=[]}}};
expr(#k_catch{anno=A,body=Kb}=Catch, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the catch.
    %% Add catch tag 'variable'.
    Cdb0 = vdb_sub(I, I+1, Vdb),
    {Es,_,Cdb1} = body(Kb, I+1, add_var({catch_tag,I}, I, locked, Cdb0)),
    L = #l{i=I,vdb=Cdb1,a=A#k.a},
    Catch#k_catch{anno=L,body=#cg_block{es=Es}};
expr(#k_receive{anno=A,var=V,body=Kb,action=Ka}=Recv, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Rdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, add_element(V#k_var.name, A#k.us), I+1,
              new_vars([V#k_var.name], I, Rdb)),
    {Tes,_,Adb} = body(Ka, I+1, Rdb),
    Le = #l{i=I,vdb=use_vars(A#k.us, I+1, Vdb),a=A#k.a},
    Recv#k_receive{anno=Le,body=M,
                   action=#cg_block{anno=#l{i=I+1,vdb=Adb,a=[]},es=Tes}};
expr(#k_receive_accept{anno=A}, I, _Vdb) ->
    #k_receive_accept{anno=#l{i=I,a=A#k.a}};
expr(#k_receive_next{anno=A}, I, _Vdb) ->
    #k_receive_next{anno=#l{i=I,a=A#k.a}};
expr(#k_put{anno=A}=Put, I, _Vdb) ->
    Put#k_put{anno=#l{i=I,a=A#k.a}};
expr(#k_break{anno=A}=Break, I, _Vdb) ->
    Break#k_break{anno=#l{i=I,a=A#k.a}};
expr(#k_guard_break{anno=A}=Break, I, _Vdb) ->
    Break#k_guard_break{anno=#l{i=I,a=A#k.a}};
expr(#k_return{anno=A}=Ret, I, _Vdb) ->
    Ret#k_return{anno=#l{i=I,a=A#k.a}}.

%% protected(Kprotected, I, Vdb) -> Protected.
%%  Only used in guards.

protected(#k_protected{anno=A,arg=Ts}=Prot, I, Vdb) ->
    %% Lock variables that are alive before try and used afterwards.
    %% Don't lock variables that are only used inside the protected
    %% expression.
    Pdb0 = vdb_sub(I, I+1, Vdb),
    {T,MaxI,Pdb1} = body(Ts, I+1, Pdb0),
    Pdb2 = use_vars(A#k.ns, MaxI+1, Pdb1),	%Save "return" values
    Prot#k_protected{arg=T,anno=#l{i=I,a=A#k.a,vdb=Pdb2}}.

%% match(Kexpr, [LockVar], I, Vdb) -> Expr.
%%  Convert match tree to old format.

match(#k_alt{anno=A,first=Kf,then=Kt}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    F = match(Kf, Ls, I+1, Vdb1),
    T = match(Kt, Ls, I+1, Vdb1),
    #k_alt{anno=[],first=F,then=T};
match(#k_select{anno=A,types=Kts}=Select, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Ts = [type_clause(Tc, Ls, I+1, Vdb1) || Tc <- Kts],
    Select#k_select{anno=[],types=Ts};
match(#k_guard{anno=A,clauses=Kcs}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Cs = [guard_clause(G, Ls, I+1, Vdb1) || G <- Kcs],
    #k_guard{anno=[],clauses=Cs};
match(Other, Ls, I, Vdb0) ->
    Vdb1 = use_vars(Ls, I, Vdb0),
    {B,_,Vdb2} = body(Other, I+1, Vdb1),
    Le = #l{i=I,vdb=Vdb2,a=[]},
    #cg_block{anno=Le,es=B}.

type_clause(#k_type_clause{anno=A,type=T,values=Kvs}, Ls, I, Vdb0) ->
    %%ok = io:format("life ~w: ~p~n", [?LINE,{T,Kvs}]),
    Vdb1 = use_vars(union(A#k.us, Ls), I+1, Vdb0),
    Vs = [val_clause(Vc, Ls, I+1, Vdb1) || Vc <- Kvs],
    #k_type_clause{anno=[],type=T,values=Vs}.

val_clause(#k_val_clause{anno=A,val=V,body=Kb}, Ls0, I, Vdb0) ->
    New = (get_kanno(V))#k.ns,
    Bus = (get_kanno(Kb))#k.us,
    %%ok = io:format("Ls0 = ~p, Used=~p\n  New=~p, Bus=~p\n", [Ls0,Used,New,Bus]),
    Ls1 = union(intersection(New, Bus), Ls0),	%Lock for safety
    Vdb1 = use_vars(union(A#k.us, Ls1), I+1, new_vars(New, I, Vdb0)),
    B = match(Kb, Ls1, I+1, Vdb1),
    Le = #l{i=I,vdb=use_vars(Bus, I+1, Vdb1),a=A#k.a},
    #k_val_clause{anno=Le,val=V,body=B}.

guard_clause(#k_guard_clause{anno=A,guard=Kg,body=Kb}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I+2, Vdb0),
    Gdb = vdb_sub(I+1, I+2, Vdb1),
    G = protected(Kg, I+1, Gdb),
    B = match(Kb, Ls, I+2, Vdb1),
    Le = #l{i=I,vdb=use_vars((get_kanno(Kg))#k.us, I+2, Vdb1),a=A#k.a},
    #k_guard_clause{anno=Le,guard=G,body=B}.


%% Here follows the code generator pass.
%%
%% The following assumptions have been made:
%%
%% 1. Matches, i.e. things with {match,M,Ret} wrappers, only return
%% values; no variables are exported. If the match would have returned
%% extra variables then these have been transformed to multiple return
%% values.
%%
%% 2. All BIF's called in guards are gc-safe so there is no need to
%% put thing on the stack in the guard.  While this would in principle
%% work it would be difficult to keep track of the stack depth when
%% trimming.
%%
%% The code generation uses variable lifetime information added by
%% the previous pass to save variables, allocate registers and
%% move registers to the stack when necessary.
%%
%% We try to use a consistent variable name scheme throughout.  The
%% StackReg record is always called Bef,Int<n>,Aft.

%% cg_fun([Lkexpr], [HeadVar], Vdb, State) -> {[Ainstr],State}

cg_fun(Les, Hvs, Vdb, St0) ->
    %%
    %% The pattern matching compiler (in v3_kernel) no longer
    %% provides its own catch-all clause, because the
    %% call to erlang:exit/1 caused problem when cases were
    %% used in guards. Therefore, there may be tests that
    %% cannot fail (providing that there is not a bug in a
    %% previous optimzation pass), but still need to provide
    %% a label (there are instructions, such as is_tuple/2,
    %% that do not allow {f,0}).
    %%
    %% We will generate an ultimate failure label and put it
    %% at the end of function, followed by an 'if_end' instruction.
    %% Note that and 'if_end' instruction does not need any
    %% live x registers, so it will always be safe to jump to
    %% it. (We never ever expect the jump to be taken, and in
    %% most functions there will never be any references to
    %% the label in the first place.)
    %%

    {UltimateFail,FailIs0,St1} = make_failure(ultimate_failure, St0),
    {Badarg,FailIs1,St2} = make_failure(badarg, St1),
    FailIs = FailIs0 ++ FailIs1,

    %% Create initial stack/register state, clear unused arguments.
    Bef = clear_dead(#sr{reg=foldl(fun (#k_var{name=V}, Reg) ->
                                           put_reg(V, Reg)
                                   end, [], Hvs),
                         stk=[]}, 0, Vdb),
    {B,_Aft,St} = cg_list(Les, Vdb, Bef,
                           St2#cg{bfail=0,
                                  ultimate_failure=UltimateFail,
                                  badarg_failure=Badarg}),
    Asm = [{label,0}|B++FailIs],
    finalize(Asm, St).

make_failure(Reason, St0) ->
    {Lbl,St1} = new_label(St0),
    {Dst,St} = new_ssa_var('@ssa_ret', St1),
    Is = [{label,Lbl},
          #b_set{dst=Dst,op=call,
                 args=[#b_remote{mod=#b_literal{val=erlang},
                                 name=#b_literal{val=error},
                                 arity=1},
                       #b_literal{val=Reason}]},
          #b_ret{arg=Dst}],
    {Lbl,Is,St}.

%% cg(Lkexpr, Vdb, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a kexpr.

cg(#cg_block{anno=Le,es=Es}, _Vdb, Bef, St) ->
    block_cg(Es, Le, Bef, St);
cg(#k_match{anno=Le,body=M,ret=Rs}, Vdb, Bef, St) ->
    match_cg(M, Rs, Le, Vdb, Bef, St);
cg(#k_guard_match{anno=Le,body=M,ret=Rs}, Vdb, Bef, St) ->
    guard_match_cg(M, Rs, Le, Vdb, Bef, St);
cg(#k_call{anno=Le,op=Func,args=As,ret=Rs}, Vdb, Bef, St) ->
    call_cg(Func, As, Rs, Le, Vdb, Bef, St);
cg(#k_enter{anno=Le,op=Func,args=As}, Vdb, Bef, St) ->
    enter_cg(Func, As, Le, Vdb, Bef, St);
cg(#k_bif{anno=Le}=Bif, Vdb, Bef, St) ->
    bif_cg(Bif, Le, Vdb, Bef, St);
cg(#k_receive{anno=Le,timeout=Te,var=Rvar,body=Rm,action=Tes,ret=Rs},
   Vdb, Bef, St) ->
    recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St);
cg(#k_receive_next{anno=Le}, Vdb, Bef, St) ->
    recv_next_cg(Le, Vdb, Bef, St);
cg(#k_receive_accept{}, _Vdb, Bef, St) ->
    Remove = #b_set{op=remove_message},
    {[Remove],Bef,St};
cg(#k_try{anno=Le,arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th,ret=Rs},
   Vdb, Bef, St) ->
    try_cg(Ta, Vs, Tb, Evs, Th, Rs, Le, Vdb, Bef, St);
cg(#k_try_enter{anno=Le,arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th},
   Vdb, Bef, St) ->
    try_enter_cg(Ta, Vs, Tb, Evs, Th, Le, Vdb, Bef, St);
cg(#k_catch{anno=Le,body=Cb,ret=[R]}, Vdb, Bef, St) ->
    do_catch_cg(Cb, R, Le, Vdb, Bef, St);
cg(#k_put{anno=Le,arg=Con,ret=Var},  Vdb, Bef, St) ->
    put_cg(Var, Con, Le, Vdb, Bef, St);
cg(#k_return{anno=Le,args=Rs}, Vdb, Bef, St) ->
    return_cg(Rs, Le, Vdb, Bef, St);
cg(#k_break{anno=Le,args=Bs}, Vdb, Bef, St) ->
    break_cg(Bs, Le, Vdb, Bef, St);
cg(#k_guard_break{anno=Le,args=Bs}, Vdb, Bef, St) ->
    guard_break_cg(Bs, Le, Vdb, Bef, St).

%% cg_list([Kexpr], FirstI, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.

cg_list(Kes, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
			     {Keis,Intb,Stb} = cg(Ke, Vdb, Inta, Sta),
			     {Keis,{Intb,Stb}}
		     end, {Bef,St0}, Kes),
    {Keis,Aft,St1}.

%% match_cg(Matc, [Ret], Le, Vdb, StackReg, State) ->
%%	{[Ainstr],StackReg,State}.
%%  Generate code for a match.  First save all variables on the stack
%%  that are to survive after the match.  We leave saved variables in
%%  their registers as they might actually be in the right place.

match_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {_Sis,Int0} = adjust_stack(Bef, I, I+1, Vdb),
    {B,St1} = new_label(St0),
    {Mis,Int1,St2} = match_cg(M, St1#cg.ultimate_failure,
			      Int0, St1#cg{break=B}),
    Reg = load_vars(Rs, Int1#sr.reg),

    {BreakVars,St} = new_ssa_vars(Rs, St2),
    {Mis ++ [{label,B},#cg_phi{vars=BreakVars}],
     clear_dead(Int1#sr{reg=Reg}, I, Vdb),
     St#cg{break=St1#cg.break}}.

guard_match_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {B,St1} = new_label(St0),
    Fail = case St0 of
               #cg{bfail=0,ultimate_failure=Fail0} -> Fail0;
               #cg{bfail=Fail0} -> Fail0
           end,
    {Mis,Aft,St2} = match_cg(M, Fail, Bef, St1#cg{break=B}),
    {BreakVars,St} = new_ssa_vars(Rs, St2),
    %% Update the register descriptors for the return registers.
    Reg = guard_match_regs(Aft#sr.reg, Rs),
    {Mis ++ [{label,B},#cg_phi{vars=BreakVars}],
     clear_dead(Aft#sr{reg=Reg}, I, Vdb),
     St#cg{break=St1#cg.break}}.

guard_match_regs([{I,gbreakvar}|Rs], [#k_var{name=V}|Vs]) ->
    [{I,V}|guard_match_regs(Rs, Vs)];
guard_match_regs([R|Rs], Vs) ->
    [R|guard_match_regs(Rs, Vs)];
guard_match_regs([], []) -> [].


%% match_cg(Match, Fail, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a match tree.  N.B. there is no need pass Vdb
%%  down as each level which uses this takes its own internal Vdb not
%%  the outer one.

match_cg(#k_alt{first=F,then=S}, Fail, Bef, St0) ->
    {Tf,St1} = new_label(St0),
    {Fis,Faft,St2} = match_cg(F, Tf, Bef, St1),
    {Sis,Saft,St3} = match_cg(S, Fail, Bef, St2),
    Aft = sr_merge(Faft, Saft),
    {Fis ++ [{label,Tf}] ++ Sis,Aft,St3};
match_cg(#k_select{var=#k_var{}=V,types=Scs}, Fail, Bef, St) ->
    match_fmf(fun (S, F, Sta) ->
		      select_cg(S, V, F, Fail, Bef, Sta) end,
	      Fail, St, Scs);
match_cg(#k_guard{clauses=Gcs}, Fail, Bef, St) ->
    match_fmf(fun (G, F, Sta) -> guard_clause_cg(G, F, Bef, Sta) end,
	      Fail, St, Gcs);
match_cg(#cg_block{anno=Le,es=Es}, _Fail, Bef, St) ->
    %% Must clear registers and stack of dead variables.
    Int = clear_dead(Bef, Le#l.i, Le#l.vdb),
    block_cg(Es, Le, Int, St).

%% block_cg([Kexpr], Le, StackReg, St) -> {[Ainstr],StackReg,St}.

block_cg(Es, Le, Bef, St) ->
    cg_list(Es, Le#l.vdb, Bef, St).

%% reserve(Bef) -> Aft.
%%  Try to reserve more registers. The registers we wish to reserve
%%  are found in Bef#sr.res.

reserve(#sr{reg=Regs,stk=Stk,res=Res}=Sr) ->
    Sr#sr{reg=reserve_1(Res, Regs, Stk)}.

reserve_1([{I,V}|Rs], [free|Regs], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [{I,V}|Regs], Stk) ->
    [{I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [{I,Var}|Regs], Stk) ->
    case on_stack(Var, Stk) of
	true -> [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
	false -> [{I,Var}|reserve_1(Rs, Regs, Stk)]
    end;
reserve_1([{I,V}|Rs], [{reserved,I,_}|Regs], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, [], Stk)];
reserve_1([], Regs, _) -> Regs.


%% select_cg(Sclause, V, TypeFail, ValueFail, StackReg, State) ->
%%      {Is,StackReg,State}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, it will always fail.

select_cg(#k_type_clause{type=k_binary,values=[S]}, Var, Tf, Vf, Bef, St) ->
    select_binary(S, Var, Tf, Vf, Bef, St);
select_cg(#k_type_clause{type=k_bin_seg,values=Vs}, Var, Tf, _Vf, Bef, St) ->
    select_bin_segs(Vs, Var, Tf, Bef, St);
select_cg(#k_type_clause{type=k_bin_int,values=Vs}, Var, Tf, _Vf, Bef, St) ->
    select_bin_segs(Vs, Var, Tf, Bef, St);
select_cg(#k_type_clause{type=k_bin_end,values=[S]}, Var, Tf, _Vf, Bef, St) ->
    select_bin_end(S, Var, Tf, Bef, St);
select_cg(#k_type_clause{type=k_map,values=Vs}, Var, Tf, Vf, Bef, St) ->
    select_map(Vs, Var, Tf, Vf, Bef, St);
select_cg(#k_type_clause{type=Type,values=Vs}, Var, Tf, Vf, Bef, St) ->
    #k_var{name=V} = Var,
    select_cg(Type, Vs, V, Tf, Vf, Bef, St).

select_cg(k_cons, [S], V, Tf, Vf, Bef, St) ->
    select_cons(S, V, Tf, Vf, Bef, St);
select_cg(k_nil, [S], V, Tf, Vf, Bef, St) ->
    select_nil(S, V, Tf, Vf, Bef, St);
select_cg(k_literal, S, V, Tf, Vf, Bef, St) ->
    select_literal(S, V, Tf, Vf, Bef, St);
select_cg(Type, Scs, V, Tf, Vf, Bef, St0) ->
    {Vis,{Aft,St1}} =
	mapfoldl(fun (S, {Int,Sta}) ->
			 {Val,Is,Inta,Stb} = select_val(S, V, Vf, Bef, Sta),
			 {{Is,[Val]},{sr_merge(Int, Inta),Stb}}
		 end, {void,St0}, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    Arg = ssa_var(V, St2),
    {Is,St} = select_val_cg(Type, Arg, Vls, Tf, Vf, Sis, St2),
    {Is,Aft,St}.

select_val_cg(k_tuple, Tuple, Vls, Tf, Vf, Sis, St0) ->
    {Is0,St1} = make_cond_branch(is_tuple, [Tuple], Tf, St0),
    {Arity,St2} = new_ssa_var('@ssa_arity', St1),
    GetArity = #b_set{dst=Arity,op={bif,tuple_size},args=[Tuple]},
    {Is,St} = select_val_cg(k_int, Arity, Vls, Tf, Vf, Sis, St2),
    {Is0++[GetArity]++Is,St};
select_val_cg(Type, R, Vls, Tf, Vf, Sis, St0) ->
    case Vls of
        [{Val,Succ}] ->
            {Is,St} = make_cond({bif,'=:='}, [R,Val], Vf, Succ, St0),
            {Is++Sis,St};
        _ ->
            Test = select_type_test(Type),
            {Is0,St1} = make_cond_branch(Test, [R], Tf, St0),
            {Is0++[#b_switch{bool=R,fail=Vf,list=Vls}|Sis],St1}
    end.

select_type_test(k_int) -> is_integer;
select_type_test(k_atom) -> is_atom;
select_type_test(k_float) -> is_float.

combine([{Is,Vs1},{Is,Vs2}|Vis]) -> combine([{Is,Vs1 ++ Vs2}|Vis]);
combine([V|Vis]) -> [V|combine(Vis)];
combine([]) -> [].

select_labels([{Is,Vs}|Vis], St0, Vls, Sis) ->
    {Lbl,St1} = new_label(St0),
    select_labels(Vis, St1, add_vls(Vs, Lbl, Vls), [[{label,Lbl}|Is]|Sis]);
select_labels([], St, Vls, Sis) ->
    {Vls,append(Sis),St}.

add_vls([V|Vs], Lbl, Acc) ->
    add_vls(Vs, Lbl, [{#b_literal{val=V},Lbl}|Acc]);
add_vls([], _, Acc) -> Acc.

select_literal(S, V, Tf, Vf, Bef, St) ->
    Src = ssa_var(V, St),
    F = fun(ValClause, Fail, St0) ->
                {Val,ValIs,Aft,St1} = select_val(ValClause, V, Vf, Bef, St0),
                Args = [Src,#b_literal{val=Val}],
                {Is,St2} = make_cond_branch({bif,'=:='}, Args, Fail, St1),
                {Is++ValIs,Aft,St2}
        end,
    match_fmf(F, Tf, St, S).

select_cons(#k_val_clause{val=#k_cons{hd=Hd,tl=Tl},body=B,anno=#l{i=I,vdb=Vdb}},
            V, Tf, Vf, Bef, St0) ->
    Es = [Hd,Tl],
    {Eis,Int,St1} = select_extract_cons(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),

    %% Real(ish) code.
    Src = ssa_var(V, St2),
    {Is,St} = make_cond_branch(is_nonempty_list, [Src], Tf, St2),
    {Is ++ Eis ++ Bis,Aft,St}.

select_nil(#k_val_clause{val=#k_nil{},body=B}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    Src = ssa_var(V, St1),
    {Is,St} = make_cond_branch(is_nil, [Src], Tf, St1),
    {Is ++ Bis,Aft,St}.

select_binary(#k_val_clause{val=#k_binary{segs=#k_var{name=Ctx0}},body=B,
                            anno=#l{i=I,vdb=Vdb}}, #k_var{anno=Anno0}=Src,
              Tf, Vf, Bef, St0) ->
    Anno = [{reuse_for_context,member(reuse_for_context, Anno0)}],
    #cg{ctx=OldCtx} = St0,
    {Ctx,St1} = new_ssa_var(Ctx0, St0),
    Regs = put_reg(Ctx0, Bef#sr.reg),
    Int0 = clear_dead(Bef#sr{reg=Regs}, I, Vdb),
    {Bis0,Aft,St2} = match_cg(B, Vf, Int0, St1#cg{ctx=Ctx}),
    {TestIs,St} = make_cond_branch(succeeded, [Ctx], Tf, St2),
    Bis1 = [#b_set{anno=Anno,dst=Ctx,op=bs_start_match,
                   args=[ssa_arg(Src, St)]}] ++ TestIs ++ Bis0,
    Bis = fix_bs_match_strings(Bis1),
    {Bis,Aft,St#cg{ctx=OldCtx}}.

fix_bs_match_strings([#b_set{op=bs_match_string,
                             args=[Ctx,#b_literal{val=BinList}]}=Set|Is])
  when is_list(BinList) ->
    I = Set#b_set{args=[Ctx,#b_literal{val=list_to_bitstring(BinList)}]},
    [I|fix_bs_match_strings(Is)];
fix_bs_match_strings([I|Is]) ->
    [I|fix_bs_match_strings(Is)];
fix_bs_match_strings([]) -> [].

make_cond(Cond, Args, Fail, Succ, St0) ->
    {Bool,St} = new_ssa_var('@ssa_bool', St0),
    Bif = #b_set{dst=Bool,op=Cond,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br],St}.

make_cond_branch(Cond, Args, Fail, St0) ->
    {Bool,St1} = new_ssa_var('@ssa_bool', St0),
    {Succ,St} = new_label(St1),
    Bif = #b_set{dst=Bool,op=Cond,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br,{label,Succ}],St}.

make_uncond_branch(Fail) ->
    #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail}.

%% New instructions for selection of binary segments.

select_bin_segs(Scs, Ivar, Tf, Bef, St) ->
    match_fmf(fun(S, Fail, Sta) ->
		      select_bin_seg(S, Ivar, Fail, Bef, Sta) end,
	      Tf, St, Scs).

select_bin_seg(#k_val_clause{val=#k_bin_seg{size=Size,unit=U,type=T,
                                            seg=Seg,flags=Fs,next=Next},
                             body=B,
                             anno=#l{i=I,vdb=Vdb,a=Anno}},
               #k_var{}=Src, Fail, Bef, St0) ->
    LineAnno = line_anno(Anno),
    Ctx = St0#cg.ctx,
    Es = case Next of
             [] -> [Seg];
             _ -> [Seg,Next]
         end,
    {Mis,Int,St1} = select_extract_bin(Es, Size, U, T, Fs, Fail,
				       I, Vdb, Bef, Ctx, LineAnno, St0),
    St2 = case Next of
              [] ->
                  St1;
              #k_var{name=NextVar} ->
                  set_ssa_var(NextVar, ssa_arg(Seg, St1), St1)
          end,
    {Bis,Aft,St} = match_cg(B, Fail, Int, St2),
    Restore = #b_set{op=bs_restore,args=[Ctx,ssa_arg(Src, St)]},
    Is = [Restore|Mis++Bis],
    {Is,Aft,St};
select_bin_seg(#k_val_clause{val=#k_bin_int{size=Sz,unit=U,flags=Fs,
                                            val=Val,next=Next},
                             body=B,
                             anno=#l{i=I,vdb=Vdb}},
               #k_var{}=Src, Fail, Bef, St0) ->
    Ctx = St0#cg.ctx,
    {Mis,Int,St1} = select_extract_int(Next, Val, Sz, U, Fs, Fail,
				       I, Vdb, Bef, Ctx, St0),
    {Bis,Aft,St} = match_cg(B, Fail, Int, St1),
    Is = case Mis ++ Bis of
             [#b_set{dst=SavePoint,op=bs_match_string,args=[OtherCtx,Bin1]},
              #b_set{dst=Bool1,op=succeeded},
              #b_br{bool=Bool1,succ=Succ,fail=Fail},
              {label,Succ},
              #b_set{op=bs_restore,args=[OtherCtx,SavePoint]},
              #b_set{op=bs_match_string,args=[OtherCtx,Bin2]}|
              [#b_set{dst=Bool2,op=succeeded},
               #b_br{bool=Bool2,fail=Fail}|_]=Is0] ->
                 %% We used to do this optimization later, but it
                 %% turns out that in huge functions with many
                 %% string matching instructions, it's a huge win
                 %% to do the combination now. To avoid copying the
                 %% binary data again and again, we'll combine bitstrings
                 %% in a list and convert all of it to a bitstring later.
                 {#b_literal{val=B1},#b_literal{val=B2}} = {Bin1,Bin2},
                 Bin = #b_literal{val=[B1,B2]},
                 [#b_set{op=bs_match_string,args=[OtherCtx,Bin]}|Is0];
             Is0 ->
                 Is0
         end,
    {[#b_set{op=bs_restore,args=[Ctx,ssa_arg(Src, St)]}|Is],Aft,St}.

select_bin_end(#k_val_clause{val=#k_bin_end{},body=B}, Src, Tf, Bef, St0) ->
    Ctx = St0#cg.ctx,
    {Bis,Aft,St1} = match_cg(B, Tf, Bef, St0),
    {TestIs,St} = make_cond_branch(bs_test_tail, [Ctx,#b_literal{val=0}], Tf, St1),
    {[#b_set{op=bs_restore,args=[Ctx,ssa_arg(Src, St)]}|TestIs++Bis],
     Aft,St}.

select_extract_bin([#k_var{name=Hd}|_], Size0, Unit, Type, Flags, Vf,
		   I, Vdb, Bef, Ctx, Anno, St0) ->
    {Dst,St1} = new_ssa_var(Hd, St0),
    Size = ssa_arg(Size0, St0),
    Reg = put_reg(Hd, Bef#sr.reg),
    Aft = Bef#sr{reg=Reg},
    {Is,St} = build_bs_instr(Anno, Type, Vf, Ctx, Size, Unit, Flags, Dst, St1),
    {Is,clear_dead(Aft, I, Vdb),St}.

select_extract_int(#k_var{name=Tl}, Val, #k_int{val=Sz}, U, Fs, Vf,
                   I, Vdb, Bef, Ctx, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_match_string', St0),
    St2 = set_ssa_var(Tl, Dst, St1),
    Bits = U*Sz,
    Bin = case member(big, Fs) of
              true ->
                  <<Val:Bits>>;
              false ->
                  true = member(little, Fs),	%Assertion.
                  <<Val:Bits/little>>
	  end,
    Bits = bit_size(Bin),			%Assertion.
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Vf, St2),
    Set = #b_set{dst=Dst,op=bs_match_string,args=[Ctx,#b_literal{val=Bin}]},
    {[Set|TestIs],clear_dead(Bef, I, Vdb),St}.

build_bs_instr(Anno, Type, Fail, Ctx, Size, Unit0, Flags0, Dst, St0) ->
    Unit = #b_literal{val=Unit0},
    Flags = #b_literal{val=Flags0},
    NeedSize = bs_need_size(Type),
    TypeArg = #b_literal{val=Type},
    Get = case NeedSize of
              true ->
                  #b_set{anno=Anno,dst=Dst,op=bs_get,
                         args=[TypeArg,Ctx,Flags,Size,Unit]};
              false ->
                  #b_set{anno=Anno,dst=Dst,op=bs_get,
                         args=[TypeArg,Ctx,Flags]}
          end,
    {Is,St} = make_cond_branch(succeeded, [Dst], Fail, St0),
    {[Get|Is],St}.

select_val(#k_val_clause{val=#k_tuple{es=Es},body=B,anno=#l{i=I,vdb=Vdb}},
           V, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_tuple(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {length(Es),Eis ++ Bis,Aft,St2};
select_val(#k_val_clause{val=Val0,body=B}, _V, Vf, Bef, St0) ->
    Val = case Val0 of
              #k_atom{val=Lit} -> Lit;
              #k_float{val=Lit} -> Lit;
              #k_int{val=Lit} -> Lit;
              #k_literal{val=Lit} -> Lit
          end,
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {Val,Bis,Aft,St1}.

%% select_extract_tuple(Src, [V], I, Vdb, StackReg, State) ->
%%      {[E],StackReg,State}.
%%  Extract tuple elements, but only if they do not immediately die.

select_extract_tuple(Src, Vs, _I, _Vdb, Bef, St0) ->
    Tuple = ssa_var(Src, St0),
    F = fun (#k_var{name=V}, {Int0,Elem,S0}) ->
                Reg1 = put_reg(V, Int0#sr.reg),
                Int1 = Int0#sr{reg=Reg1},
                Args = [Tuple,#b_literal{val=Elem}],
                {Dst,S} = new_ssa_var(V, S0),
                Get = #b_set{dst=Dst,op=get_tuple_element,args=Args},
                {[Get],{Int1,Elem+1,S}}
	end,
    {Es,{Aft,_,St}} = flatmapfoldl(F, {Bef,0,St0}, Vs),
    {Es,Aft,St}.

select_map(Scs, V, Tf, Vf, Bef, St0) ->
    MapSrc = ssa_arg(V, St0),
    {Is,Aft,St1} =
	match_fmf(fun(#k_val_clause{val=#k_map{op=exact,es=Es},
                                    body=B}, Fail, St1) ->
                          select_map_val(V, Es, B, Fail, Bef, St1)
                  end, Vf, St0, Scs),
    {TestIs,St} = make_cond_branch(is_map, [MapSrc], Tf, St1),
    {TestIs++Is,Aft,St}.

select_map_val(V, Es, B, Fail, Bef, St0) ->
    {Eis,Int,St1} = select_extract_map(Es, V, Fail, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    {Eis++Bis,Aft,St2}.

select_extract_map([P|Ps], Src, Fail, Bef, St0) ->
    MapSrc = ssa_arg(Src, St0),
    #k_map_pair{key=Key0,val=#k_var{name=Dst0}} = P,
    Reg1 = put_reg(Dst0, Bef#sr.reg),
    Int = Bef#sr{reg=Reg1},
    Key = ssa_arg(Key0, St0),
    {Dst,St1} = new_ssa_var(Dst0, St0),
    Set = #b_set{dst=Dst,op=get_map_element,args=[MapSrc,Key]},
    {TestIs,St2} = make_cond_branch(succeeded, [Dst], Fail, St1),
    {Is,Aft,St} = select_extract_map(Ps, Src, Fail, Int, St2),
    {[Set|TestIs]++Is,Aft,St};
select_extract_map([], _, _, Bef, St) ->
    {[],Bef,St}.

select_extract_cons(Src0, [#k_var{name=Hd},#k_var{name=Tl}], I, Vdb, Bef, St0) ->
    case {vdb_find(Hd, Vdb), vdb_find(Tl, Vdb)} of
        {{_,_,Lhd}, {_,_,Ltl}} when Lhd =< I, Ltl =< I ->
            %% Both head and tail are dead.  No need to generate
            %% any instruction.
            {[],Bef,St0};
        _ ->
            Reg0 = put_reg(Tl, put_reg(Hd, Bef#sr.reg)),
            Int0 = Bef#sr{reg=Reg0},
            Aft = clear_dead(Int0, I, Vdb),

            %% Real(ish) code.
            Src = ssa_var(Src0, St0),
            {HdDst,St1} = new_ssa_var(Hd, St0),
            {TlDst,St2} = new_ssa_var(Tl, St1),
            GetHd = #b_set{op=get_hd,dst=HdDst,args=[Src]},
            GetTl = #b_set{op=get_tl,dst=TlDst,args=[Src]},
            {[GetHd,GetTl],Aft,St2}
    end.

guard_clause_cg(#k_guard_clause{anno=#l{vdb=Vdb},guard=G,body=B}, Fail, Bef, St0) ->
    {Gis,Int,St1} = guard_cg(G, Fail, Vdb, Bef, St0),
    {Bis,Aft,St} = match_cg(B, Fail, Int, St1),
    {Gis ++ Bis,Aft,St}.

%% guard_cg(Guard, Fail, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%  A guard is a boolean expression of tests.  Tests return true or
%%  false.  A fault in a test causes the test to return false.  Tests
%%  never return the boolean, instead we generate jump code to go to
%%  the correct exit point.  Primops and tests all go to the next
%%  instruction on success or jump to a failure label.

guard_cg(#k_protected{arg=Ts,ret=Rs,anno=#l{vdb=Pdb}}, Fail, _Vdb, Bef, St) ->
    protected_cg(Ts, Rs, Fail, Pdb, Bef, St);
guard_cg(#k_test{anno=#l{i=I},op=Test0,args=As,inverted=Inverted},
         Fail, Vdb, Bef, St0) ->
    #k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Test}} = Test0,
    test_cg(Test, Inverted, As, Fail, I, Vdb, Bef, St0);
guard_cg(G, _Fail, Vdb, Bef, St) ->
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{G,Fail,Vdb,Bef}]),
    {Gis,Aft,St1} = cg(G, Vdb, Bef, St),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Aft}]),
    {Gis,Aft,St1}.

test_cg(Test, Inverted, As0, Fail, I, Vdb, Bef, St0) ->
    Aft = clear_dead(Bef, I, Vdb),
    As = ssa_args(As0, St0),
    {Bool,St1} = new_ssa_var('@ssa_bool', St0),
    {Succ,St} = new_label(St1),
    Bif = #b_set{op=Test,dst=Bool,args=As},
    Br = case Inverted of
             false -> #b_br{bool=Bool,succ=Succ,fail=Fail};
             true -> #b_br{bool=Bool,succ=Fail,fail=Succ}
         end,
    {[Bif,Br,{label,Succ}],Aft,St}.

%% guard_cg_list([Kexpr], Fail, I, Vdb, StackReg, St) ->
%%      {[Ainstr],StackReg,St}.

guard_cg_list(Kes, Fail, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
			     {Keis,Intb,Stb} =
				 guard_cg(Ke, Fail, Vdb, Inta, Sta),
			     {Keis,{Intb,Stb}}
		     end, {Bef,St0}, Kes),
    {Keis,Aft,St1}.

%% protected_cg([Kexpr], [Ret], Fail, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Do a protected.  Protecteds without return values are just done
%%  for effect, the return value is not checked, success passes on to
%%  the next instruction and failure jumps to Fail.  If there are
%%  return values then these must be set to 'false' on failure,
%%  control always passes to the next instruction.

protected_cg(Ts, [], Fail, Vdb, Bef, St0) ->
    %% Protect these calls, revert when done.
    {Tis,Aft,St1} = guard_cg_list(Ts, Fail, Vdb, Bef, St0#cg{bfail=Fail}),
    {Tis,Aft,St1#cg{bfail=St0#cg.bfail}};
protected_cg(Ts, Rs, _Fail, Vdb, Bef, St0) ->
    {Pfail,St1} = new_label(St0),
    {Psucc,St2} = new_label(St1),
    {Tis,Aft,St3} = guard_cg_list(Ts, Pfail, Vdb, Bef,
				  St2#cg{bfail=Pfail}),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Rs,I,Vdb,Aft}]),
    %% Set return values to false.
    %% FIXME: Must rewrite this.
    error(nyi),
    Mis = [{move,{atom,false},fetch_var(V,Aft)}||#k_var{name=V} <- Rs],
    {Tis ++ [{jump,{f,Psucc}},
	     {label,Pfail}] ++ Mis ++ [{label,Psucc}],
     Aft,St3#cg{bfail=St0#cg.bfail}}.

%% match_fmf(Fun, LastFail, State, [Clause]) -> {Is,Aft,State}.
%%  This is a special flatmapfoldl for match code gen where we
%%  generate a "failure" label for each clause. The last clause uses
%%  an externally generated failure label, LastFail.  N.B. We do not
%%  know or care how the failure labels are used.

match_fmf(F, LastFail, St, [H]) ->
    F(H, LastFail, St);
match_fmf(F, LastFail, St0, [H|T]) ->
    {Fail,St1} = new_label(St0),
    {R,Aft1,St2} = F(H, Fail, St1),
    {Rs,Aft2,St3} = match_fmf(F, LastFail, St2, T),
    {R ++ [{label,Fail}] ++ Rs,sr_merge(Aft1, Aft2),St3}.

%% call_cg(Func, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%% enter_cg(Func, [Arg], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Call and enter first put the arguments into registers and save any
%%  other registers, then clean up and compress the stack and set the
%%  frame size. Finally the actual call is made.  Call then needs the
%%  return values filled in.

call_cg(Func, As, [], Le, Vdb, Bef, St) ->
    call_cg(Func, As, [#k_var{name='@ssa_ignored'}], Le, Vdb, Bef, St);
call_cg(Func0, As, [#k_var{name=R}|MoreRs]=Rs, Le, Vdb, Bef, St0) ->
    case St0 of
        #cg{bfail=Fail} when Fail =/= 0 ->
            %% Inside a guard. The only allowed function call is to
            %% erlang:error/1,2. We will generate a branch to the
            %% failure branch.
            #k_remote{mod=#k_atom{val=erlang},
                      name=#k_atom{val=error}} = Func0, %Assertion.
            [#k_var{name=DestVar}] = Rs,
            Int0 = clear_dead(Bef, Le#l.i, Vdb),
            Reg = put_reg(DestVar, Int0#sr.reg),
            Int = Int0#sr{reg=Reg},
            St = set_ssa_var(DestVar, #b_literal{val=unused}, St0),
            {[make_uncond_branch(Fail),
              #cg_unreachable{}],
             clear_dead(Int, Le#l.i, Vdb),St};
	#cg{} ->
	    %% Ordinary function call in a function body.

            %% FIXME: To be removed.
	    {_Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
	    %% Put return values in registers.
	    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
	    Aft = clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),

            %% Real(ish) code.
            Args = ssa_args(As, St0),
            {Ret,St1} = new_ssa_var(R, St0),
            Func = call_target(Func0, Args, St0),
            Call = #b_set{anno=line_anno(Le),op=call,dst=Ret,args=[Func|Args]},

            %% FIXME: Dummy variables are only need when there is a
            %% call erlang:error(). Instead of generating the dummy variables,
            %% we should eliminate branch out the block (perhaps best done in
            %% finalize()).
            St = foldl(fun(#k_var{name=Dummy}, S) ->
                               set_ssa_var(Dummy, #b_literal{val=unused}, S)
                       end, St1, MoreRs),
            {[Call],Aft,St}
    end.

enter_cg(Func0, As0, Le, Vdb, Bef, St0) ->
    {_Sis,Aft} = cg_setup_call(As0, Bef, Le#l.i, Vdb),

    %% Real(ish) code.
    Anno = line_anno(Le),
    Func = call_target(Func0, As0, St0),
    As = ssa_args(As0, St0),
    {Ret,St} = new_ssa_var('@ssa_ret', St0),
    Call = #b_set{anno=Anno,op=call,dst=Ret,args=[Func|As]},
    {[Call,#b_ret{arg=Ret}],
     clear_dead(Aft#sr{reg=clear_regs(Aft#sr.reg)}, Le#l.i, Vdb),
     St}.

call_target(Func, As, St) ->
    Arity = length(As),
    case Func of
        #k_remote{mod=Mod0,name=Name0} ->
            Mod = ssa_arg(Mod0, St),
            Name = ssa_arg(Name0, St),
            #b_remote{mod=Mod,name=Name,arity=Arity};
        #k_local{name=Name} when is_atom(Name) ->
            #b_local{name=Name,arity=Arity};
        #k_var{name=V} ->
            ssa_var(V, St)
    end.

%% bif_cg(#k_bif{}, Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%  Generate code a BIF.

bif_cg(#k_bif{op=#k_internal{name=Name},args=As,ret=Rs}, Le, Vdb, Bef, St) ->
    internal_cg(Name, As, Rs, Le, Vdb, Bef, St);
bif_cg(#k_bif{op=#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Name}},
              args=As,ret=Rs}, Le, Vdb, Bef, St) ->
    bif_cg(Name, As, Rs, Le, Vdb, Bef, St).

%% internal_cg(Bif, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

internal_cg(bs_context_to_binary, [Src0], [], Le, Vdb, Bef, St0) ->
    {Dst,St} = new_ssa_var('@ssa_binary', St0),
    Src = ssa_arg(Src0, St),
    Set = #b_set{dst=Dst,op=context_to_binary,args=[Src]},
    {[Set],clear_dead(Bef, Le#l.i, Vdb), St};
internal_cg(dsetelement, [Index0,Tuple0,New0], _Rs, Le, Vdb, Bef, St) ->
    [New,Tuple,#b_literal{val=Index1}] = ssa_args([New0,Tuple0,Index0], St),
    Index = #b_literal{val=Index1-1},
    Set = #b_set{op=dsetelement,args=[New,Tuple,Index]},
    {[Set],clear_dead(Bef, Le#l.i, Vdb),St};
internal_cg(make_fun, [Name0,Arity0|As], Rs, Le, Vdb, Bef, St0) ->
    #k_atom{val=Name} = Name0,
    #k_int{val=Arity} = Arity0,
    {_Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),

    %% Real(ish) code.
    [#k_var{name=Dst0}] = Rs,
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Local = #b_local{name=Name,arity=Arity},
    MakeFun = #b_set{dst=Dst,op=make_fun,args=[Local|Args]},

    {[MakeFun],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St};
internal_cg(bs_init_writable=I, As, [#k_var{name=Dst0}]=Rs, Le, Vdb, Bef, St0) ->
    %% This behaves like a function call.
    {_Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{dst=Dst,op=I,args=Args},
    {[Set],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St};
internal_cg(build_stacktrace=I, As, [#k_var{name=Dst0}]=Rs, Le, Vdb, Bef, St0) ->
    {_Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{dst=Dst,op=I,args=Args},
    {[Set],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St};
internal_cg(raise, As, Rs, Le, Vdb, Bef, St) ->
    %% raise can be treated like a guard BIF.
    bif_cg(raise, As, Rs, Le, Vdb, Bef, St);
internal_cg(raw_raise=I, As, [#k_var{name=Dst0}]=Rs, Le, Vdb, Bef, St0) ->
    %% This behaves like a function call.
    {_Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{dst=Dst,op=I,args=Args},
    {[Set],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St}.

bif_cg(Bif, As0, [#k_var{name=Dst0}], Le, Vdb, Bef, #cg{bfail=Fail}=St0) ->
    MayFail = not erl_bifs:is_safe(erlang, Bif, length(As0)),
    {_Sis,Int0} =
	case MayFail of
	    true ->
		maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0);
	    false ->
		{[],Bef}
	end,
    Int1 = clear_dead(Int0, Le#l.i, Vdb),
    Reg = put_reg(Dst0, Int1#sr.reg),
    Int = Int1#sr{reg=Reg},
    Aft = clear_dead(Int, Le#l.i, Vdb),

    %% Real code.
    As = ssa_args(As0, St0),
    {Dst,St1} = new_ssa_var(Dst0, St0),
    I = #b_set{anno=line_anno(Le),dst=Dst,op={bif,Bif},args=As},
    case MayFail of
        true when Fail =/= 0->
            {Is,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
            {[I|Is],Aft,St};
        _ ->
            {[I],Aft,St1}
    end.

%% recv_loop_cg(TimeOut, ReceiveVar, ReceiveMatch, TimeOutExprs,
%%              [Ret], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St0) ->
    {_Sis,Int0} = adjust_stack(Bef, Le#l.i, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=clear_regs(Int0#sr.reg)},
    %% Get labels.
    {Rl,St1} = new_label(St0),
    {Tl,St2} = new_label(St1),
    {Bl,St3} = new_label(St2),
    St4 = St3#cg{break=Bl,recv=Rl},
    {Ris,Raft,St5} = cg_recv_mesg(Rvar, Rm, Tl, Int1, Le, St4),
    {Wis,Taft,St6} = cg_recv_wait(Te, Tes, Le#l.i, Int1, St5),
    Int2 = sr_merge(Raft, Taft),		%Merge stack/registers
    Reg = load_vars(Rs, Int2#sr.reg),
    {BreakVars,St} = new_ssa_vars(Rs, St6),
    {Ris ++ [{label,Tl}] ++ Wis ++
         [{label,Bl},#cg_phi{vars=BreakVars}],
     clear_dead(Int2#sr{reg=Reg}, Le#l.i, Vdb),
     St#cg{break=St0#cg.break,recv=St0#cg.recv}}.

%% cg_recv_mesg( ) -> {[Ainstr],Aft,St}.

cg_recv_mesg(#k_var{name=R}, Rm, Tl, Bef, Le, St0) ->
    Int0 = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Int1 = Int0,
    {Dst,St1} = new_ssa_var(R, St0),
    {Mis,Int2,St2} = match_cg(Rm, none, Int1, St1),
    RecvLbl = St1#cg.recv,
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Tl, St2),
    Is = [#b_br{anno=line_anno(Le),bool=#b_literal{val=true},
                succ=RecvLbl,fail=RecvLbl},
          {label,RecvLbl},
          #b_set{dst=Dst,op=peek_message,args=[]}|TestIs],
    {Is++Mis,Int2,St}.

%% cg_recv_wait(Te, Tes, I, Vdb, Int2, St3) -> {[Ainstr],Aft,St}.

cg_recv_wait(#k_atom{val=infinity}, #cg_block{anno=Le,es=Tes}, I, Bef, St0) ->
    %% We know that the 'after' body will never be executed.
    %% But to keep the stack and register information up to date,
    %% we will generate the code for the 'after' body, and then discard it.
    Int1 = clear_dead(Bef, I, Le#l.vdb),
    {_,Int2,St} = cg_list(Tes, Le#l.vdb,
                          Int1#sr{reg=clear_regs(Int1#sr.reg)}, St0),
    Is = [#b_set{op=wait},make_uncond_branch(St#cg.recv)],
    {Is,Int2,St};
cg_recv_wait(#k_int{val=0}, #cg_block{anno=Le,es=Tes}, _I, Bef, St0) ->
    {Tis,Int,St} = cg_list(Tes, Le#l.vdb, Bef, St0),
    {[#b_set{op=timeout}|Tis],Int,St};
cg_recv_wait(Te, #cg_block{anno=Le,es=Tes}, I, Bef, St0) ->
    %% Must have empty registers here!  Bug if anything in registers.
    Int0 = clear_dead(Bef, I, Le#l.vdb),
    {Tis,Int,St1} = cg_list(Tes, Le#l.vdb,
                            Int0#sr{reg=clear_regs(Int0#sr.reg)}, St0),
    Args = [ssa_arg(Te, St1)],
    {WaitDst,St2} = new_ssa_var('@ssa_wait', St1),
    {WaitIs,St} = make_cond_branch(succeeded, [WaitDst], St1#cg.recv, St2),
    Is = [#b_set{dst=WaitDst,op=wait_timeout,args=Args}] ++ WaitIs ++
        [#b_set{op=timeout}] ++Tis,
    {Is,Int,St}.

%% recv_next_cg(Le, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.
%%  Use adjust stack to clear stack, but only need it for Aft.

recv_next_cg(Le, Vdb, Bef, St) ->
    {_Sis,Aft} = adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb),
    Is = [#b_set{op=recv_next},make_uncond_branch(St#cg.recv)],
    {Is,Aft,St}.

%% try_cg(TryBlock, [BodyVar], TryBody, [ExcpVar], TryHandler, [Ret],
%%        Le, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.

try_cg(Ta, Vs, Tb, Evs, Th, Rs, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2} = new_label(St1),			%Handler label
    {E,St3aa} = new_label(St2),			%End label
    {Next,St3} = new_label(St3aa),
    #l{i=TryTag} = get_kanno(Ta),
    Int1 = Bef#sr{stk=put_catch(TryTag, Bef#sr.stk)},
    {TryReg,St3a} = new_ssa_var('@ssa_catch_tag', St3),
    {SsaVs,St3b} = new_ssa_vars(Vs, St3a),
    {SsaEvs,St3c} = new_ssa_vars(Evs, St3b),
    {Ais,Int2,St4} = cg(Ta, Vdb, Int1, St3c#cg{break=B,in_catch=true}),
    Int3 = Int2#sr{stk=drop_catch(TryTag, Int2#sr.stk)},
    St5 = St4#cg{break=E,in_catch=St3#cg.in_catch},
    {Bis,Baft,St6} = cg(Tb, Vdb, Int3#sr{reg=load_vars(Vs, Int3#sr.reg)}, St5),
    {His,Haft,St7} = cg(Th, Vdb, Int3#sr{reg=load_vars(Evs, Int3#sr.reg)}, St6),
    Int4 = sr_merge(Baft, Haft),		%Merge stack/registers
    Aft = Int4#sr{reg=load_vars(Rs, Int4#sr.reg)},
    {BreakVars,St8} = new_ssa_vars(Rs, St7),
    {CatchedAgg,St} = new_ssa_var('@ssa_agg', St8),
    ExtractVs = extract_vars(SsaEvs, CatchedAgg, 0),
    Handler = [{label,H},
               #b_set{dst=CatchedAgg,op=try_case,args=[TryReg]}|ExtractVs],
    {[#b_set{dst=TryReg,op=new_try_tag,args=[]},
      #b_br{bool=TryReg,succ=Next,fail=H},{label,Next}] ++ Ais ++
         [{label,B},#cg_phi{vars=SsaVs},
          #b_set{op=try_end,args=[TryReg]}] ++ Bis ++
         Handler ++ His ++
         [{label,E},#cg_phi{vars=BreakVars}],
     clear_dead(Aft, Le#l.i, Vdb),
     St#cg{break=St0#cg.break}}.

try_enter_cg(Ta, Vs, Tb, Evs, Th, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2aa} = new_label(St1),			%Handler label
    {Next,St2} = new_label(St2aa),
    #l{i=TryTag} = get_kanno(Ta),
    Int1 = Bef#sr{stk=put_catch(TryTag, Bef#sr.stk)},
    {TryReg,St2a} = new_ssa_var('@ssa_catch_tag', St2),
    {SsaVs,St2b} = new_ssa_vars(Vs, St2a),
    {SsaEvs,St2c} = new_ssa_vars(Evs, St2b),
    {Ais,Int2,St3} = cg(Ta, Vdb, Int1, St2c#cg{break=B,in_catch=true}),
    Int3 = Int2#sr{stk=drop_catch(TryTag, Int2#sr.stk)},
    St4 = St3#cg{in_catch=St2#cg.in_catch},
    {Bis,Baft,St5} = cg(Tb, Vdb, Int3#sr{reg=load_vars(Vs, Int3#sr.reg)}, St4),
    {His,Haft,St6} = cg(Th, Vdb, Int3#sr{reg=load_vars(Evs, Int3#sr.reg)}, St5),
    Int4 = sr_merge(Baft, Haft),		%Merge stack/registers
    Aft = Int4,
    {CatchedAgg,St} = new_ssa_var('@ssa_agg', St6),
    ExtractVs = extract_vars(SsaEvs, CatchedAgg, 0),
    Handler = [{label,H},
               #b_set{dst=CatchedAgg,op=try_case,args=[TryReg]}|ExtractVs],
    {[#b_set{dst=TryReg,op=new_try_tag,args=[]},
      #b_br{bool=TryReg,succ=Next,fail=H},{label,Next}] ++
         Ais ++ [{label,B},#cg_phi{vars=SsaVs},
                 #b_set{op=try_end,args=[TryReg]}] ++ Bis ++
         Handler ++ His,clear_dead(Aft, Le#l.i, Vdb),
     St#cg{break=St0#cg.break}}.

extract_vars([V|Vs], Agg, N) ->
    I = #b_set{dst=V,op=extract,args=[Agg,#b_literal{val=N}]},
    [I|extract_vars(Vs, Agg, N+1)];
extract_vars([], _, _) -> [].

%% do_catch_cg(CatchBlock, Ret, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

do_catch_cg(#cg_block{es=C}, #k_var{name=R}, Le, Vdb, Bef, St0) ->
    {B,St1aa} = new_label(St0),
    {Next,St1} = new_label(St1aa),
    CatchTag = Le#l.i,
    Int1 = Bef#sr{stk=put_catch(CatchTag, Bef#sr.stk)},
    {CatchReg,St1a} = new_ssa_var('@ssa_catch_tag', St1),
    {Dst,St1b} = new_ssa_var(R, St1a),
    {Cis,Int2,St2} = cg_list(C, Le#l.vdb, Int1, St1b#cg{break=B,in_catch=true}),
    [] = Int2#sr.reg,				%Assertion.
    Aft = Int2#sr{reg=[{0,R}],stk=drop_catch(CatchTag, Int2#sr.stk)},

    {[#b_set{dst=CatchReg,op=new_try_tag,args=[]},
      #b_br{bool=CatchReg,succ=Next,fail=B},{label,Next}] ++ Cis ++
         [{label,B},#cg_phi{vars=[Dst]},#b_set{op=catch_end,args=[CatchReg]}],
     clear_dead(Aft, Le#l.i, Vdb),
     St2#cg{break=St1#cg.break,in_catch=St1#cg.in_catch}}.

%% put_cg([Var], Constr, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  We have to be careful how a 'put' works. First the structure is
%%  built, then it is filled and finally things can be cleared. The
%%  annotation must reflect this and make sure that the return
%%  variable is allocated first.
%%
%%  put_list and put_map are atomic instructions, both of
%%  which can safely resuse one of the source registers as target.

put_cg([#k_var{name=R}], #k_cons{hd=Hd,tl=Tl}, Le, Vdb, Bef, St0) ->
    Int0 = clear_dead(Bef, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=put_reg(R, Int0#sr.reg)},

    %% Real(ish) code.
    Args = ssa_args([Hd,Tl], St0),
    {Dst,St} = new_ssa_var(R, St0),
    PutList = #b_set{op=put_list,dst=Dst,args=Args},
    {[PutList],Int1,St};
put_cg([#k_var{name=R}], #k_tuple{es=Es}, Le, Vdb, Bef, St0) ->
    Int = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    {Ret,St} = new_ssa_var(R, St0),
    Args = ssa_args(Es, St),
    PutTuple = #b_set{op=put_tuple,dst=Ret,args=Args},
    {[PutTuple],clear_dead(Int, Le#l.i, Vdb),St};
put_cg([#k_var{name=R}], #k_binary{segs=Segs}, Le, Vdb, Bef,
       #cg{bfail=Bfail}=St0) ->
    %% At run-time, binaries are constructed in three stages:
    %% 1) First the size of the binary is calculated.
    %% 2) Then the binary is allocated.
    %% 3) Then each field in the binary is constructed.
    %% For simplicity, we use the target register to also hold the
    %% size of the binary. Therefore the target register must *not*
    %% be one of the source registers.

    %% First allocate the target register.
    Int0 = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    {Dst,St1} = new_ssa_var(R, St0),

    %% First generate the code that constructs each field.
    Fail = if
               Bfail =:= 0 -> St1#cg.badarg_failure;
               true -> Bfail
           end,
    {PutCode,SzCalc,St2} = cg_bin_put(Segs, Fail, St1),
    {_Sis,Int1} = maybe_adjust_stack(Int0, Le#l.i, Le#l.i+1, Vdb, St2),
    Aft = clear_dead(Int1, Le#l.i, Vdb),

    %% Now generate the complete code for constructing the binary.
    {BinDst,BinIs,St3} = cg_binary(PutCode, SzCalc, Dst, Fail, Le#l.a, St2),
    {TestIs,St} = make_cond_branch(succeeded, [BinDst], Fail, St3),
    {BinIs++TestIs,Aft,St};
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,
                                es=[#k_map_pair{key=#k_var{}=K,val=V}]},
       Le, Vdb, Bef, St0) ->
    %% Map: single variable key.
    {_Sis,Int0} = maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0),

    SrcMap = ssa_arg(Map, St0),
    LineAnno = line_anno(Le#l.a),
    List = [ssa_arg(K, St0),ssa_arg(V, St0)],

    %% The target register can reuse one of the source registers.
    Aft0 = clear_dead(Int0, Le#l.i, Vdb),
    Aft = Aft0#sr{reg=put_reg(R, Aft0#sr.reg)},

    {Dst,St1} = new_ssa_var(R, St0),
    {Is,St} = put_cg_map(LineAnno, Op, SrcMap, Dst, List, St1),
    {Is,Aft,St};
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,es=Es}, Le, Vdb, Bef, St0) ->
    %% Map: one or more literal keys.
    [] = [Var || #k_map_pair{key=#k_var{}=Var} <- Es], %Assertion

    {_Sis,Int0} = maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0),

    SrcMap = ssa_arg(Map, St0),
    LineAnno = line_anno(Le#l.a),
    List = flatmap(fun(#k_map_pair{key=K,val=V}) ->
                           [ssa_arg(K, St0),ssa_arg(V, St0)]
                   end, Es),

    Aft0 = clear_dead(Int0, Le#l.i, Vdb),
    Aft = Aft0#sr{reg=put_reg(R, Aft0#sr.reg)},

    {Dst,St1} = new_ssa_var(R, St0),
    {Is,St} = put_cg_map(LineAnno, Op, SrcMap, Dst, List, St1),
    {Is,Aft,St};
put_cg([#k_var{name=R}], Con0, Le, Vdb, Bef, St0) ->
    Int = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Con = ssa_arg(Con0, St0),
    St = set_ssa_var(R, Con, St0),
    {[],clear_dead(Int, Le#l.i, Vdb),St}.

put_cg_map(LineAnno, Op0, SrcMap, Dst, List, St0) ->
    Bfail = St0#cg.bfail,
    Op = case Op0 of
	     assoc -> put_map_assoc;
	     exact -> put_map_exact
	 end,
    {_OkLbl,St1} = new_label(St0),
    {_BadLbl,St2} = new_label(St1),
    PutMap = #b_set{anno=LineAnno,dst=Dst,op=Op,args=[SrcMap|List]},
    if
        Bfail =:= 0 orelse Op =:= put_map_assoc ->
            {[PutMap],St2};
        true ->
            {Is,St} = make_cond_branch(succeeded, [Dst], Bfail, St2),
            {[PutMap|Is],St}
    end.

%%%
%%% Code generation for constructing binaries.
%%%

cg_binary(PutCode, SzCalc, Dst, Fail, Anno, St0) ->
    LineAnno = line_anno(Anno),
    {Unit,SzVar,SzCode,St} = cg_size_calc(8, SzCalc, Fail, St0),
    [#b_set{op=bs_put,args=PutArgs}|_] = PutCode,
    Args = case PutArgs of
               [_,_,Src,#b_literal{val=all}|_] ->
                   case member(single_use, Anno) of
                       true ->
                           [#b_literal{val=private_append},
                            Src,Unit,SzVar];
                       false ->
                           [#b_literal{val=append},Src,SzVar,Unit]
                   end;
               [_|_] ->
                   [#b_literal{val=new},SzVar,Unit]
           end,
    BsInit = #b_set{anno=LineAnno,dst=Dst,op=bs_init,args=Args},
    {Dst,SzCode ++ [BsInit|PutCode],St}.

%% cg_size_calc(PreferredUnit, SzCalc, St0) ->
%%         {ActualUnit,SizeVariable,SizeCode,St}.
%%  Generate size calculation code.

cg_size_calc(Unit, error, Fail, St) ->
    Pre = [make_uncond_branch(Fail),#cg_unreachable{}],
    {#b_literal{val=Unit},#b_literal{val=badarg},Pre,St};
cg_size_calc(8, [{1,_}|_]=SzCalc, Fail, St) ->
    cg_size_calc(1, SzCalc, Fail, St);
cg_size_calc(8, SzCalc, Fail, St0) ->
    {Var,Pre,St} = cg_size_calc_1(SzCalc, Fail, St0),
    {#b_literal{val=8},Var,Pre,St};
cg_size_calc(1, SzCalc0, Fail, St0) ->
    SzCalc = map(fun({8,#b_literal{val=Size}}) ->
                         {1,#b_literal{val=8*Size}};
                    ({8,{{bif,byte_size},Src}}) ->
                         {1,{{bif,bit_size},Src}};
                    ({8,{_,_}=UtfCalc}) ->
                         {1,{'*',#b_literal{val=8},UtfCalc}};
                    ({_,_}=Pair) ->
                         Pair
                 end, SzCalc0),
    {Var,Pre,St} = cg_size_calc_1(SzCalc, Fail, St0),
    {#b_literal{val=1},Var,Pre,St}.

cg_size_calc_1(SzCalc, Fail, St0) ->
    cg_size_calc_2(SzCalc, #b_literal{val=0}, Fail, St0).

cg_size_calc_2([{_,{'*',Unit,{_,_}=Bif}}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, Fail, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, Unit, Fail, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([{_,#b_literal{}=Sz}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, 1, Fail, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,#b_var{}=Sz}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, 1, Fail, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,{_,_}=Bif}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, Fail, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, 1, Fail, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([], Sum, _Fail, St) ->
    {Sum,[],St}.

cg_size_bif({Name,Src}, Fail, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_bif', St0),
    Bif = #b_set{dst=Dst,op=Name,args=[Src]},
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
    {Dst,[Bif|TestIs],St}.

cg_size_add(#b_literal{val=0}, Val, 1, _Fail, St) ->
    {Val,[],St};
cg_size_add(A, B, Unit, Fail, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_sum', St0),
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
    BsAdd = #b_set{dst=Dst,op=bs_add,args=[A,B,#b_literal{val=Unit}]},
    {Dst,[BsAdd|TestIs],St}.

cg_bin_put(Seg, Fail, St) ->
    cg_bin_put_1(Seg, Fail, [], [], St).

cg_bin_put_1(#k_bin_seg{size=Size0,unit=U,type=T,flags=Fs,seg=Src0,next=Next},
           Fail, Acc, SzCalcAcc, St0) ->
    [Src,Size] = ssa_args([Src0,Size0], St0),
    NeedSize = bs_need_size(T),
    TypeArg = #b_literal{val=T},
    Flags = #b_literal{val=Fs},
    Unit = #b_literal{val=U},
    Args = case NeedSize of
               true -> [TypeArg,Flags,Src,Size,Unit];
               false -> [TypeArg,Flags,Src]
           end,
    {Is,St} = make_cond_branch(bs_put, Args, Fail, St0),
    SzCalc = bin_size_calc(T, Src, Size, U),
    cg_bin_put_1(Next, Fail, reverse(Is, Acc), [SzCalc|SzCalcAcc], St);
cg_bin_put_1(#k_bin_end{}, _, Acc, SzCalcAcc, St) ->
    SzCalc = fold_size_calc(SzCalcAcc, 0, []),
    {reverse(Acc),SzCalc,St}.

bs_need_size(utf8) -> false;
bs_need_size(utf16) -> false;
bs_need_size(utf32) -> false;
bs_need_size(_) -> true.

bin_size_calc(utf8, Src, _Size, _Unit) ->
    {8,{bs_utf8_size,Src}};
bin_size_calc(utf16, Src, _Size, _Unit) ->
    {8,{bs_utf16_size,Src}};
bin_size_calc(utf32, _Src, _Size, _Unit) ->
    {8,#b_literal{val=4}};
bin_size_calc(binary, Src, #b_literal{val=all}, Unit) ->
    case Unit rem 8 of
        0 -> {8,{{bif,byte_size},Src}};
        _ -> {1,{{bif,bit_size},Src}}
    end;
bin_size_calc(_Type, _Src, Size, Unit) ->
    {Unit,Size}.

fold_size_calc([{Unit,#b_literal{val=Size}}|T], Bits, Acc) ->
    if
        is_integer(Size) ->
            fold_size_calc(T, Bits + Unit*Size, Acc);
        true ->
            error
    end;
fold_size_calc([{_,_}=H|T], Bits, Acc) ->
    fold_size_calc(T, Bits, [H|Acc]);
fold_size_calc([], Bits, Acc) ->
    Bytes = Bits div 8,
    RemBits = Bits rem 8,
    Sizes = sort([{1,#b_literal{val=RemBits}},{8,#b_literal{val=Bytes}}|Acc]),
    [Pair || {_,Sz}=Pair <- Sizes, Sz =/= #b_literal{val=0}].

%% return_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%% break_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  These are very simple, just put return/break values in registers
%%  from 0, then return/break.  Use the call setup to clean up stack,
%%  but must clear registers to ensure sr_merge works correctly.

return_cg(Rs, Le, Vdb, Bef, St) ->
    {_,Int} = cg_setup_call(Rs, Bef, Le#l.i, Vdb),
    [Ret] = ssa_args(Rs, St),
    {[#b_ret{arg=Ret}],Int#sr{reg=clear_regs(Int#sr.reg)},St}.

break_cg(Bs, Le, Vdb, Bef, #cg{break=Br}=St) ->
    {_Ms,Int} = cg_setup_call(Bs, Bef, Le#l.i, Vdb),
    Args = ssa_args(Bs, St),
    {[#cg_break{args=Args,phi=Br}],Int#sr{reg=clear_regs(Int#sr.reg)},St}.

guard_break_cg(Bs, #l{i=I}, Vdb, Bef, #cg{break=Br}=St) ->
    #sr{reg=Reg1} = Int = clear_dead(Bef, I, Vdb),
    Reg2 = trim_free(Reg1),
    {BreakVars,_} = mapfoldl(fun(_, RegNum) ->
				     {{RegNum,gbreakvar},RegNum+1}
			     end, length(Reg2), Bs),
    Reg = Reg2 ++ BreakVars,
    Aft = Int#sr{reg=Reg},
    Args = ssa_args(Bs, St),
    {[#cg_break{args=Args,phi=Br}],Aft,St}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FIXME: The register state handling is kept for now mainly
%%% as a sanity check. Ultimately, all of it will be removed.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cg_setup_call([Arg], Bef, Cur, Vdb) -> {[Instr],Aft}.
%%  Do the complete setup for a call/enter.

cg_setup_call(As, Bef, I, Vdb) ->
    {Ms,Int0} = cg_call_args(As, Bef, I, Vdb),
    %% Have set up arguments, can now clean up, compress and save to stack.
    Int1 = Int0#sr{stk=clear_dead_stk(Int0#sr.stk, I, Vdb),res=[]},
    {Sis,Int2} = adjust_stack(Int1, I, I+1, Vdb),
    {Ms ++ Sis,Int2}.

%% cg_call_args([Arg], SrState) -> {[Instr],SrState}.
%%  Setup the arguments to a call/enter/bif. Put the arguments into
%%  consecutive registers starting at {x,0} moving any data which
%%  needs to be saved. Return a modified SrState structure with the
%%  new register contents.  N.B. the resultant register info will
%%  contain non-variable values when there are non-variable values.
%%
%%  This routine is complicated by unsaved values in x registers.
%%  We'll move away any unsaved values that are in the registers
%%  to be overwritten by the arguments.

cg_call_args(As, Bef, I, Vdb) ->
    Regs0 = load_arg_regs(Bef#sr.reg, As),
    Unsaved = unsaved_registers(Regs0, Bef#sr.stk, I, I+1, Vdb),
    {UnsavedMoves,Regs} = move_unsaved(Unsaved, Bef#sr.reg, Regs0),
    Moves0 = gen_moves(As, Bef),
    Moves = order_moves(Moves0, find_scratch_reg(Regs)),
    {UnsavedMoves ++ Moves,Bef#sr{reg=Regs}}.

%% load_arg_regs([Reg], Arguments) -> [Reg]
%%  Update the register descriptor to include the arguments (from {x,0}
%%  and upwards). Values in argument register are overwritten.
%%  Values in x registers above the arguments are preserved.

load_arg_regs(Regs, As) -> load_arg_regs(Regs, As, 0).

load_arg_regs([_|Rs], [#k_var{name=V}|As], I) -> [{I,V}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([_|Rs], [A|As], I) -> [{I,A}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([], [#k_var{name=V}|As], I) -> [{I,V}|load_arg_regs([], As, I+1)];
load_arg_regs([], [A|As], I) -> [{I,A}|load_arg_regs([], As, I+1)];
load_arg_regs(Rs, [], _) -> Rs.

%% Returns the variables must be saved and are currently in the
%% x registers that are about to be overwritten by the arguments.

unsaved_registers(Regs, Stk, Fb, Lf, Vdb) ->
    [V || {V,F,L} <- Vdb,
	  F < Fb,
	  L >= Lf,
	  not on_stack(V, Stk),
	  not in_reg(V, Regs)].

in_reg(V, Regs) -> keymember(V, 2, Regs).

%% Move away unsaved variables from the registers that are to be
%% overwritten by the arguments.
move_unsaved(Vs, OrigRegs, NewRegs) ->
    move_unsaved(Vs, OrigRegs, NewRegs, []).

move_unsaved([V|Vs], OrigRegs, NewRegs0, Acc) ->
    NewRegs = put_reg(V, NewRegs0),
    Src = fetch_reg(V, OrigRegs),
    Dst = fetch_reg(V, NewRegs),
    move_unsaved(Vs, OrigRegs, NewRegs, [{move,Src,Dst}|Acc]);
move_unsaved([], _, Regs, Acc) -> {Acc,Regs}.

%% gen_moves(As, Sr)
%%  Generate the basic move instruction to move the arguments
%%  to their proper registers. The list will be sorted on
%%  destinations. (I.e. the move to {x,0} will be first --
%%  see the comment to order_moves/2.)

gen_moves(As, Sr) -> gen_moves(As, Sr, 0, []).

gen_moves([#k_var{name=V}|As], Sr, I, Acc) ->
    case fetch_var(V, Sr) of
	{x,I} -> gen_moves(As, Sr, I+1, Acc);
	Reg -> gen_moves(As, Sr, I+1, [{move,Reg,{x,I}}|Acc])
    end;
gen_moves([A0|As], Sr, I, Acc) ->
    A = atomic(A0),
    gen_moves(As, Sr, I+1, [{move,A,{x,I}}|Acc]);
gen_moves([], _, _, Acc) -> lists:keysort(3, Acc).

%% order_moves([Move], ScratchReg) -> [Move]
%%  Orders move instruction so that source registers are not
%%  destroyed before they are used. If there are cycles
%%  (such as {move,{x,0},{x,1}}, {move,{x,1},{x,1}}),
%%  the scratch register is used to break up the cycle.
%%    If possible, the first move of the input list is placed
%%  last in the result list (to make the move to {x,0} occur
%%  just before the call to allow the Beam loader to coalesce
%%  the instructions).

order_moves(Ms, Scr) -> order_moves(Ms, Scr, []).

order_moves([{move,_,_}=M|Ms0], ScrReg, Acc0) ->
    {Chain,Ms} = collect_chain(Ms0, [M], ScrReg),
    Acc = reverse(Chain, Acc0),
    order_moves(Ms, ScrReg, Acc);
order_moves([], _, Acc) -> Acc.

collect_chain(Ms, Path, ScrReg) ->
    collect_chain(Ms, Path, [], ScrReg).

collect_chain([{move,Src,Same}=M|Ms0], [{move,Same,_}|_]=Path, Others, ScrReg) ->
    case lists:keyfind(Src, 3, Path) of
	false ->
	    collect_chain(reverse(Others, Ms0), [M|Path], [], ScrReg);
	_ ->	% We have a cycle.
	    {break_up_cycle(M, Path, ScrReg),reverse(Others, Ms0)}
    end;
collect_chain([M|Ms], Path, Others, ScrReg) ->
    collect_chain(Ms, Path, [M|Others], ScrReg);
collect_chain([], Path, Others, _) ->
    {Path,Others}.

break_up_cycle({move,Src,_}=M, Path, ScrReg) ->
    [{move,ScrReg,Src},M|break_up_cycle1(Src, Path, ScrReg)].

break_up_cycle1(Dst, [{move,Src,Dst}|Path], ScrReg) ->
    [{move,Src,ScrReg}|Path];
break_up_cycle1(Dst, [M|Path], LastMove) ->
    [M|break_up_cycle1(Dst, Path, LastMove)].

%% clear_dead(Sr, Until, Vdb) -> Aft.
%%  Remove all variables in Sr which have died AT ALL so far.

clear_dead(#sr{stk=Stk}=Sr0, Until, Vdb) ->
    Sr = Sr0#sr{reg=clear_dead_reg(Sr0, Until, Vdb),
                stk=clear_dead_stk(Stk, Until, Vdb)},
    reserve(Sr).

clear_dead_reg(Sr, Until, Vdb) ->
    [case R of
         {_I,V} = IV ->
             case vdb_find(V, Vdb) of
                 {V,_,L} when L > Until -> IV;
                 _ -> free                      %Remove anything else
             end;
         {reserved,_I,_V}=Reserved -> Reserved;
         free -> free
     end || R <- Sr#sr.reg].

clear_dead_stk(Stk, Until, Vdb) ->
    [case S of
	    {V} = T ->
            case vdb_find(V, Vdb) of
                {V,_,L} when L > Until -> T;
                _ -> dead   %Remove anything else
            end;
        free -> free;
        dead -> dead
     end || S <- Stk].


%% sr_merge(Sr1, Sr2) -> Sr.
%%  Merge two stack/register states keeping the longest of both stack
%%  and register. Perform consistency check on both, elements must be
%%  the same.  Allow frame size 'void' to make easy creation of
%%  "empty" frame.

sr_merge(#sr{reg=R1,stk=S1,res=[]}, #sr{reg=R2,stk=S2,res=[]}) ->
    #sr{reg=longest(R1, R2),stk=longest(S1, S2),res=[]};
sr_merge(void, S2) -> S2#sr{res=[]}.

longest([H|T1], [H|T2]) -> [H|longest(T1, T2)];
longest([dead|T1], [free|T2]) -> [dead|longest(T1, T2)];
longest([free|T1], [dead|T2]) -> [dead|longest(T1, T2)];
longest([dead|_] = L, []) -> L;
longest([], [dead|_] = L) -> L;
longest([free|_] = L, []) -> L;
longest([], [free|_] = L) -> L;
longest([], []) -> [].

trim_free([R|Rs0]) ->
    case {trim_free(Rs0),R} of
	{[],free} -> [];
	{Rs,R} -> [R|Rs]
    end;
trim_free([]) -> [].

%% maybe_adjust_stack(Bef, FirstBefore, LastFrom, Vdb, St) -> {[Ainstr],Aft}.
%%  Adjust the stack, but only if the code is inside a catch and not
%%  inside a guard.  Use this funtion before instructions that may
%%  cause an exception.

maybe_adjust_stack(Bef, Fb, Lf, Vdb, St) ->
    case St of
	#cg{in_catch=true,bfail=0} ->
	    adjust_stack(Bef, Fb, Lf, Vdb);
	#cg{} ->
	    {[],Bef}
    end.

%% adjust_stack(Bef, FirstBefore, LastFrom, Vdb) -> {[Ainstr],Aft}.
%%  Do complete stack adjustment by compressing stack and adding
%%  variables to be saved.  Try to optimise ordering on stack by
%%  having reverse order to their lifetimes.
%%
%%  In Beam, there is a fixed stack frame and no need to do stack compression.

adjust_stack(Bef, Fb, Lf, Vdb) ->
    Stk0 = Bef#sr.stk,
    {Stk1,Saves} = save_stack(Stk0, Fb, Lf, Vdb),
    {saves(Saves, Bef#sr.reg, Stk1),
     Bef#sr{stk=Stk1}}.

%% save_stack(Stack, FirstBefore, LastFrom, Vdb) -> {[SaveVar],NewStack}.
%%  Save variables which are used past current point and which are not
%%  already on the stack.

save_stack(Stk0, Fb, Lf, Vdb) ->
    %% New variables that are in use but not on stack.
    New = new_not_on_stack(Stk0, Fb, Lf, Vdb),

    %% Add new variables that are not just dropped immediately.
    %% N.B. foldr works backwards from the end!!
    Saves = [V || {V,_,_} <- keysort(3, New)],
    Stk1 = foldr(fun (V, Stk) -> put_stack(V, Stk) end, Stk0, Saves),
    {Stk1,Saves}.

%% new_not_on_stack(Stack, FirstBefore, LastFrom, Vdb) ->
%%                 [{Variable,First,Last}]
%%  Return information about all variables that are used past current
%%  point and that are not already on the stack.

new_not_on_stack(Stk, Fb, Lf, Vdb) ->
    [VFL || {V,F,L} = VFL <- Vdb,
            F < Fb,
            L >= Lf,
            not on_stack(V, Stk)].

%% saves([SaveVar], Reg, Stk) -> [{move,Reg,Stk}].
%%  Generate move instructions to save variables onto stack.  The
%%  stack/reg info used is that after the new stack has been made.

saves(Ss, Reg, Stk) ->
    [{move,fetch_reg(V, Reg),fetch_stack(V, Stk)} || V <- Ss].

%% fetch_var(VarName, StkReg) -> r{R} | sp{Sp}.
%% find_var(VarName, StkReg) -> ok{r{R} | sp{Sp}} | error.
%%  Fetch/find a variable in either the registers or on the
%%  stack. Fetch KNOWS it's there.

fetch_var(V, Sr) ->
    case find_reg(V, Sr#sr.reg) of
	{ok,R} -> R;
	error -> fetch_stack(V, Sr#sr.stk)
    end.

load_vars(Vs, Regs) ->
    foldl(fun (#k_var{name=V}, Rs) -> put_reg(V, Rs) end, Regs, Vs).

%% put_reg(Val, Regs) -> Regs.
%% find_reg(Val, Regs) -> {ok,r{R}} | error.
%% fetch_reg(Val, Regs) -> r{R}.
%%  Functions to interface the registers.

put_reg(V, Rs) -> put_reg_1(V, Rs, 0).

put_reg_1(V, [free|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [{reserved,I,V}|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [R|Rs], I) -> [R|put_reg_1(V, Rs, I+1)];
put_reg_1(V, [], I) -> [{I,V}].

fetch_reg(V, [{I,V}|_]) -> {x,I};
fetch_reg(V, [_|SRs]) -> fetch_reg(V, SRs).

find_reg(V, [{I,V}|_]) -> {ok,{x,I}};
find_reg(V, [_|SRs]) -> find_reg(V, SRs);
find_reg(_, []) -> error.

%% For the bit syntax, we need a scratch register if we are constructing
%% a binary that will not be used.

find_scratch_reg(Rs) -> find_scratch_reg(Rs, 0).

find_scratch_reg([free|_], I) -> {x,I};
find_scratch_reg([_|Rs], I) -> find_scratch_reg(Rs, I+1);
find_scratch_reg([], I) -> {x,I}.

clear_regs(_) -> [].

%% put_stack(Val, [{Val}]) -> [{Val}].
%% fetch_stack(Var, Stk) -> sp{S}.
%% find_stack(Var, Stk) -> ok{sp{S}} | error.
%%  Functions to interface the stack.

put_stack(Val, []) -> [{Val}];
put_stack(Val, [dead|Stk]) -> [{Val}|Stk];
put_stack(Val, [free|Stk]) -> [{Val}|Stk];
put_stack(Val, [NotFree|Stk]) -> [NotFree|put_stack(Val, Stk)].

fetch_stack(Var, Stk) -> fetch_stack(Var, Stk, 0).

fetch_stack(V, [{V}|_], I) -> {yy,I};
fetch_stack(V, [_|Stk], I) -> fetch_stack(V, Stk, I+1).

on_stack(V, Stk) -> keymember(V, 1, Stk).

%% put_catch(CatchTag, Stack) -> Stack'
%% drop_catch(CatchTag, Stack) -> Stack'
%%  Special interface for putting and removing catch tags, to ensure that
%%  catches nest properly. Also used for try tags.

put_catch(Tag, Stk0) -> put_catch(Tag, reverse(Stk0), []).

put_catch(Tag, [], Stk) ->
    put_stack({catch_tag,Tag}, Stk);
put_catch(Tag, [{{catch_tag,_}}|_]=RevStk, Stk) ->
    reverse(RevStk, put_stack({catch_tag,Tag}, Stk));
put_catch(Tag, [Other|Stk], Acc) ->
    put_catch(Tag, Stk, [Other|Acc]).

drop_catch(Tag, [{{catch_tag,Tag}}|Stk]) -> [free|Stk];
drop_catch(Tag, [Other|Stk]) -> [Other|drop_catch(Tag, Stk)].

%% atomic(Klit) -> Lit.
%% atomic_list([Klit]) -> [Lit].

atomic(#k_literal{val=V}) -> {literal,V};
atomic(#k_int{val=I}) -> {integer,I};
atomic(#k_float{val=F}) -> {float,F};
atomic(#k_atom{val=A}) -> {atom,A};
%%atomic(#k_char{val=C}) -> {char,C};
atomic(#k_nil{}) -> nil.

ssa_args(As, St) ->
    [ssa_arg(A, St) || A <- As].

ssa_var(V, #cg{vars=Vars}) -> maps:get(V, Vars).

ssa_arg(#k_var{name=V}, St) -> ssa_var(V, St);
ssa_arg(#k_literal{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_atom{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_float{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_int{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_nil{}, _) -> #b_literal{val=[]}.

init_ssa_args(Vs, St) ->
    mapfoldl(fun(#k_var{name=V}, S) ->
                     new_ssa_var(V, S)
             end, St, Vs).

new_ssa_vars(Vs, St) ->
    new_ssa_vars(Vs, [], St).

new_ssa_vars([#k_var{name=V0}|Vs], Acc, St0) ->
    {V,St} = new_ssa_var(V0, St0),
    new_ssa_vars(Vs, [V|Acc], St);
new_ssa_vars([], Acc, St) ->
    {reverse(Acc),St}.

new_ssa_var(VarBase, #cg{lcount=Uniq,vars=Vars}=St0)
  when is_atom(VarBase); is_integer(VarBase) ->
    case Vars of
        #{VarBase:=_} ->
            Var = #b_var{name={VarBase,Uniq}},
            St = St0#cg{lcount=Uniq+1,vars=Vars#{VarBase=>Var}},
            {Var,St};
        #{} ->
            Var = #b_var{name=VarBase},
            St = St0#cg{vars=Vars#{VarBase=>Var}},
            {Var,St}
    end.

set_ssa_var(VarBase, Val, #cg{vars=Vars}=St)
  when is_atom(VarBase); is_integer(VarBase) ->
    St#cg{vars=Vars#{VarBase=>Val}}.

%% new_label(St) -> {L,St}.

new_label(#cg{lcount=Next}=St) ->
    {Next,St#cg{lcount=Next+1}}.

%% line(Le) -> {line,[] | {location,File,Line}}
%%  Create a line instruction, containing information about
%%  the current filename and line number. A line information
%%  instruction should be placed before any operation that could
%%  cause an exception.

line_anno(Le) ->
    {line,Line} = line(Le),
    Line.

line(#l{a=Anno}) ->
    line(Anno);
line([Line,{file,Name}]) when is_integer(Line) ->
    line_1(Name, Line);
line([_|_]=A) ->
    {Name,Line} = find_loc(A, no_file, 0),
    line_1(Name, Line);
line([]) ->
    {line,[]}.

line_1(no_file, _) ->
    {line,[]};
line_1(_, 0) ->
    %% Missing line number or line number 0.
    {line,[]};
line_1(Name, Line) ->
    {line,[{location,Name,Line}]}.

find_loc([Line|T], File, _) when is_integer(Line) ->
    find_loc(T, File, Line);
find_loc([{file,File}|T], _, Line) ->
    find_loc(T, File, Line);
find_loc([_|T], File, Line) ->
    find_loc(T, File, Line);
find_loc([], File, Line) -> {File,Line}.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.

%%%
%%% Finalize the code.
%%%

finalize(Asm0, St0) ->
    Asm1 = fix_phis(Asm0),
    {Asm,St} = fix_sets(Asm1, [], St0),
    {build_map(Asm),St}.

fix_phis(Is) ->
    fix_phis_1(Is, none, #{}).

fix_phis_1([{label,L}=I|Is], _Lbl, Map) ->
    [I|fix_phis_1(Is, L, Map)];
fix_phis_1([#cg_unreachable{}|Is0], _Lbl, Map) ->
    Is = drop_upto_label(Is0),
    fix_phis_1(Is, none, Map);
fix_phis_1([#cg_break{args=Args,phi=Target}|Is], Lbl, Map) when is_integer(Lbl) ->
    Pairs1 = case Map of
                 #{Target:=Pairs0} -> Pairs0;
                 #{} -> []
             end,
    Pairs = [[{Arg,Lbl} || Arg <- Args]|Pairs1],
    I = make_uncond_branch(Target),
    case Pairs of
        [[]] ->
            [I|fix_phis_1(Is, none, Map)];
        [_|_] ->
            [I|fix_phis_1(Is, none, Map#{Target=>Pairs})]
    end;
fix_phis_1([#cg_phi{vars=[]}|Is], Lbl, Map0) ->
    Map = maps:remove(Lbl, Map0),
    fix_phis_1(Is, Lbl, Map);
fix_phis_1([#cg_phi{vars=Vars}|Is], Lbl, Map0) ->
    Pairs = maps:get(Lbl, Map0),
    Map = maps:remove(Lbl, Map0),
    Phis = gen_phis(Vars, Pairs),
    Phis ++ fix_phis_1(Is, Lbl, Map);
fix_phis_1([I|Is], Lbl, Map) ->
    [I|fix_phis_1(Is, Lbl, Map)];
fix_phis_1([], _, Map) ->
    [] = maps:to_list(Map),                     %Assertion.
    [].

gen_phis([V|Vs], Preds0) ->
    {Pairs,Preds} = collect_preds(Preds0, [], []),
    [#b_set{dst=V,op=phi,args=Pairs}|gen_phis(Vs, Preds)];
gen_phis([], _) -> [].

collect_preds([[First|Rest]|T], ColAcc, RestAcc) ->
    collect_preds(T, [First|ColAcc], [Rest|RestAcc]);
collect_preds([], ColAcc, RestAcc) ->
    {keysort(2, ColAcc),RestAcc}.

fix_sets([#b_set{dst=none}=Set|Is], Acc, St0) ->
    {Dst,St} = new_ssa_var('@ssa_ignored', St0),
    I = Set#b_set{dst=Dst},
    fix_sets(Is, [I|Acc], St);
fix_sets([I|Is], Acc, St) ->
    fix_sets(Is, [I|Acc], St);
fix_sets([], Acc, St) ->
    {reverse(Acc),St}.

build_map(Is) ->
    Blocks = build_graph_1(Is, [], []),
    maps:from_list(Blocks).

build_graph_1([{label,L}|Is], Lbls, []) ->
    build_graph_1(Is, [L|Lbls], []);
build_graph_1([{label,L}|Is], Lbls, [_|_]=BlockAcc) ->
    make_blocks(Lbls, BlockAcc) ++ build_graph_1(Is, [L], []);
build_graph_1([I|Is], Lbls, BlockAcc) ->
    build_graph_1(Is, Lbls, [I|BlockAcc]);
build_graph_1([], Lbls, BlockAcc) ->
    make_blocks(Lbls, BlockAcc).

make_blocks(Lbls, [Last|Is0]) ->
    Is = reverse(Is0),
    Block = #b_blk{is=Is,last=Last},
    [{L,Block} || L <- Lbls].

drop_upto_label([{label,_}|_]=Is) -> Is;
drop_upto_label([_|Is]) -> drop_upto_label(Is);
drop_upto_label([]) -> [].

%% Keep track of life time for variables.
%%
%% init_vars([{var,VarName}]) -> Vdb.
%% new_vars([VarName], I, Vdb) -> Vdb.
%% use_vars([VarName], I, Vdb) -> Vdb.
%% add_var(VarName, F, L, Vdb) -> Vdb.
%%
%% The list of variable names for new_vars/3 and use_vars/3
%% must be sorted.
%%
%% FIXME: To be removed.

init_vars(Vs) ->
    vdb_new(Vs).

new_vars([], _, Vdb) -> Vdb;
new_vars([V], I, Vdb) -> vdb_store_new(V, {V,I,I}, Vdb);
new_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

use_vars([], _, Vdb) ->
    Vdb;
use_vars([V], I, Vdb) ->
    case vdb_find(V, Vdb) of
        {V,F,L} when I > L -> vdb_update(V, {V,F,I}, Vdb);
        {V,_,_} -> Vdb;
        error -> vdb_store_new(V, {V,I,I}, Vdb)
    end;
use_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

add_var(V, F, L, Vdb) ->
    vdb_store_new(V, {V,F,L}, Vdb).

%% vdb

vdb_new(Vs) ->
    ordsets:from_list([{V,0,0} || #k_var{name=V} <- Vs]).

-type var() :: atom().

-spec vdb_find(var(), [vdb_entry()]) -> 'error' | vdb_entry().

vdb_find(V, Vdb) ->
    case lists:keyfind(V, 1, Vdb) of
        false -> error;
        Vd -> Vd
    end.

vdb_update(V, Update, [{V,_,_}|Vdb]) ->
    [Update|Vdb];
vdb_update(V, Update, [Vd|Vdb]) ->
    [Vd|vdb_update(V, Update, Vdb)].

vdb_store_new(V, New, [{V1,_,_}=Vd|Vdb]) when V > V1 ->
    [Vd|vdb_store_new(V, New, Vdb)];
vdb_store_new(V, New, [{V1,_,_}|_]=Vdb) when V < V1 ->
    [New|Vdb];
vdb_store_new(_, New, []) -> [New].

vdb_update_vars([V|_]=Vs, [{V1,_,_}=Vd|Vdb], I) when V > V1 ->
    [Vd|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{V1,_,_}|_]=Vdb, I) when V < V1 ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{_,F,L}=Vd|Vdb], I) ->
    %% Existing variable.
    if
        I > L -> [{V,F,I}|vdb_update_vars(Vs, Vdb, I)];
        true ->  [Vd|vdb_update_vars(Vs, Vdb, I)]
    end;
vdb_update_vars([V|Vs], [], I) ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, [], I)];
vdb_update_vars([], Vdb, _) -> Vdb.

%% vdb_sub(Min, Max, Vdb) -> Vdb.
%%  Extract variables which are used before and after Min.  Lock
%%  variables alive after Max.

vdb_sub(Min, Max, Vdb) ->
    [ if L >= Max -> {V,F,locked};
         true -> Vd
      end || {V,F,L}=Vd <- Vdb,
             F < Min,
             L >= Min ].
