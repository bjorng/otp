%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(beam_listing).

-export([module/2]).

-include("core_parse.hrl").
-include("v3_kernel.hrl").
-include("beam_ssa.hrl").
-include("beam_disasm.hrl").

-import(lists, [foreach/2]).

-type code() :: cerl:c_module()
              | beam_utils:module_code()
              | #k_mdef{}
              | [_].                            %form-based format

-spec module(file:io_device(), code()) -> 'ok'.

module(File, #c_module{}=Core) ->
    %% This is a core module.
    io:put_chars(File, core_pp:format(Core));
module(File, #k_mdef{}=Kern) ->
    %% This is a kernel module.
    io:put_chars(File, v3_kernel_pp:format(Kern));
    %%io:put_chars(File, io_lib:format("~p~n", [Kern]));
module(File, #b_module{name=Mod,exports=Exp,attributes=Attr,body=Fs}) ->
    io:format(File, "module ~p.\n", [Mod]),
    io:format(File, "exports ~p.\n", [Exp]),
    io:format(File, "attributes ~p.\n\n", [Attr]),
    _ = [format_ssa_function(File, F) || F <- Fs],
    ok;
module(Stream, {Mod,Exp,Attr,Code,NumLabels}) ->
    %% This is output from v3_codegen.
    io:format(Stream, "{module, ~p}.  %% version = ~w\n",
	      [Mod, beam_opcodes:format_number()]),
    io:format(Stream, "\n{exports, ~p}.\n", [Exp]),
    io:format(Stream, "\n{attributes, ~p}.\n", [Attr]),
    io:format(Stream, "\n{labels, ~p}.\n", [NumLabels]),
    foreach(
      fun ({function,Name,Arity,Entry,Asm}) ->
	      io:format(Stream, "\n\n{function, ~w, ~w, ~w}.\n",
			[Name, Arity, Entry]),
	      io:put_chars(Stream, format_asm(Asm))
      end, Code);
module(Stream, [_|_]=Fs) ->
    %% Form-based abstract format.
    foreach(fun (F) -> io:format(Stream, "~p.\n", [F]) end, Fs).

format_asm([{label,L}|Is]) ->
    ["  {label,",integer_to_list(L),"}.\n"|format_asm(Is)];
format_asm([I|Is]) ->
    [io_lib:format("    ~p", [I]),".\n"|format_asm(Is)];
format_asm([]) -> [].

%%%
%%% Pretty print the SSA immediate format.
%%%

format_ssa_function(File, #b_func{anno=Anno,args=Args,
                                  bs=Blocks,cnt=Counter}) ->
    [{func_info,{{M,F,_},Loc}}] = Anno,
    io:nl(File),
    case Loc of
        [{location,Filename,Line}] ->
            io:format(File, "%% ~ts:~p\n", [Filename,Line]);
        [] ->
            ok
    end,
    io:format(File, "%% Counter = ~p\n", [Counter]),
    io:format(File, "function ~p:~p(~s) {\n", [M,F,format_args(Args)]),
    io:nl(File),
    Ws = gb_sets:singleton(0),
    Seen = gb_sets:empty(),
    ReachableBlocks = reachable_blocks(Ws, Seen, Blocks),
    format_blocks(File, ReachableBlocks, Blocks),
    All = maps:keys(Blocks),
    Unreachable = ordsets:subtract(ordsets:from_list(All),
                                   ordsets:from_list(ReachableBlocks)),
    case Unreachable of
        [] ->
            ok;
        [_|_] ->
            io:put_chars(File, "%% Unreachable blocks\n\n"),
            format_blocks(File, Unreachable, Blocks),
            io:put_chars(File, "}\n")
    end.

reachable_blocks(Ws0, Seen0, Blocks) ->
    case gb_sets:is_empty(Ws0) of
        true ->
            [];
        false ->
            {L,Ws1} = gb_sets:take_smallest(Ws0),
            Block = maps:get(L, Blocks),
            Seen = gb_sets:add(L, Seen0),
            Ws = update_ws(Block, Seen, Ws1),
            [L|reachable_blocks(Ws, Seen, Blocks)]
    end.

update_ws(#b_blk{last=Terminator}, Seen, Ws) ->
    New0 = update_ws_1(Terminator),
    New1 = gb_sets:from_list(New0),
    New = gb_sets:difference(New1, Seen),
    gb_sets:union(New, Ws).

update_ws_1(#b_br{succ=Succ,fail=Fail}) ->
    [Succ,Fail];
update_ws_1(#b_switch{fail=Fail,list=List}) ->
    [Fail|[L || {_,L} <- List]];
update_ws_1(#b_try{body=Body,handler=Fail}) ->
    [Body,Fail];
update_ws_1(#b_ret{}) ->
    [];
update_ws_1({jump,{f,L}}) ->
    %% FIXME. Kludge.
    [L].

format_blocks(File, Labels, Blocks) ->
    _ = [format_block(File, L, Blocks) || L <- Labels],
    ok.

format_block(File, L, Blocks) ->
    #b_blk{is=Is,last=Last} = maps:get(L, Blocks),
    io:format(File, "~p:\n", [L]),
    _ = [format_instr(File, I) || I <- Is],
    format_terminator(File, Last),
    io:nl(File).

format_instr(File, #b_set{anno=Anno,dst=Dst,op=Op,args=Args}) ->
    io:put_chars(File, format_anno(Anno)),
    io:format(File, "  ~s = ~s", [format_var(Dst),format_op(Op)]),
    case Args of
        [] ->
            io:nl(File);
        [_|_] ->
            io:format(File, " ~s\n", [format_args(Args)])
    end;
format_instr(File, I) ->
    io:format(File, "  *** ~p ***\n", [I]).

format_terminator(File, #b_br{bool=#b_literal{val=true},succ=Lbl,fail=Lbl}) ->
    io:format(File, "  br label ~p\n", [Lbl]);
format_terminator(File, #b_br{bool=Bool,succ=Succ,fail=Fail}) ->
    io:format(File, "  br ~s, label ~p, label ~p\n", [format_arg(Bool),Succ,Fail]);
format_terminator(File, #b_switch{bool=Bool,fail=Fail,list=List}) ->
    io:format(File, "  switch ~s, label ~p, ~s\n",
              [format_arg(Bool),Fail,format_list(List)]);
format_terminator(File, #b_try{var=Var,body=Body,handler=Fail}) ->
    io:format(File, "  try ~s, label ~p, label ~p\n",
              [format_arg(Var),Body,Fail]);
format_terminator(File, #b_ret{arg=Arg}) ->
    io:format(File, "  return ~s\n", [format_arg(Arg)]);
format_terminator(File, I) ->
    io:format(File, "  *** ~p ***\n", [I]).

format_op({bif,Name}) ->
    io_lib:format("bif:~p", [Name]);
format_op(Name) ->
    io_lib:format("~p", [Name]).

format_var(#b_var{name={Name,Uniq}}) ->
    io_lib:format("~s:~p", [Name,Uniq]);
format_var(#b_var{name=Name}) ->
    atom_to_list(Name).

format_args(Args) ->
    Ss = [format_arg(Arg) || Arg <- Args],
    lists:join(", ", Ss).

format_arg(#b_var{}=Var) ->
    format_var(Var);
format_arg(#b_literal{val=Val}) ->
    io_lib:format("literal ~p", [Val]);
format_arg(#b_remote{mod=Mod,name=Name,arity=Arity}) ->
    io_lib:format("remote (~s):(~s)/~p",
                  [format_arg(Mod),format_arg(Name),Arity]);
format_arg(#b_local{name=Name,arity=Arity}) ->
    io_lib:format("local ~s/~p", [Name,Arity]);
format_arg({Value,Label}) when is_integer(Label) ->
    io_lib:format("{ ~s, ~p }", [format_arg(Value),Label]);
format_arg(Other) ->
    io_lib:format("*** ~p ***", [Other]).

format_list(List) ->
    Ss = [io_lib:format("{ ~s, ~s }", [format_arg(Val),format_label(L)]) ||
             {Val,L} <- List],
    io_lib:format("[ ~s ]", [lists:join(", ", Ss)]).

format_label(L) ->
    ["label ",integer_to_list(L)].

format_anno([{location,File,Line}]) ->
    io_lib:format("  %% ~s:~p\n", [File,Line]);
format_anno([_|_]=Other) ->
    io_lib:format("  %% Anno: ~p\n", [Other]);
format_anno([]) ->
    [].
