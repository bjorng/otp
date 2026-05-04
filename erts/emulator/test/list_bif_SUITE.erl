%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(list_bif_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2]).
-export([hd_test/1,tl_test/1,t_length/1,t_list_to_pid/1,
         t_list_to_ref/1, t_list_to_ext_pidportref/1,
         t_list_to_port/1,t_list_to_float/1,t_list_to_float_2/1,
         t_list_to_integer/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].


all() -> 
    [hd_test, tl_test, t_length, t_list_to_pid, t_list_to_port,
     t_list_to_ref, t_list_to_ext_pidportref,
     t_list_to_float, t_list_to_float_2, t_list_to_integer].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

%% Tests list_to_integer and string:to_integer
t_list_to_integer(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch list_to_integer("12373281903728109372810937209817320981321ABC")),
    12373281903728109372810937209817320981321 = (catch list_to_integer("12373281903728109372810937209817320981321")),
    12373 = (catch list_to_integer("12373")),
    -12373 =  (catch list_to_integer("-12373")),
    12373 = (catch list_to_integer("+12373")),
    {'EXIT',{badarg,_}} = (catch list_to_integer(abc)),
    {'EXIT',{badarg,_}} = (catch list_to_integer("")),
    {12373281903728109372810937209817320981321,"ABC"} = string:to_integer("12373281903728109372810937209817320981321ABC"),
    {-12373281903728109372810937209817320981321,"ABC"} = string:to_integer("-12373281903728109372810937209817320981321ABC"),
    {12,[345]} = string:to_integer([$1,$2,345]),
    {error,badarg} = string:to_integer([$1,$2,a]),
    {error,no_integer} = string:to_integer([$A]),
    {error,badarg} = string:to_integer($A),

    %% System limit.
    Digits = lists:duplicate(3_000_000, $9),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits),
    _ = erlang:garbage_collect(),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits, 16),
    _ = erlang:garbage_collect(),
    {error,system_limit} = string:to_integer(Digits),
    _ = erlang:garbage_collect(),

    ok.

%% Test hd/1 with correct and incorrect arguments.
hd_test(Config) when is_list(Config) ->
    $h = hd(id("hejsan")),
    case catch hd(id($h)) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("hd/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test tl/1 with correct and incorrect arguments.
tl_test(Config) when is_list(Config) ->
    "ejsan" = tl(id("hejsan")),
    case catch tl(id(104)) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("tl/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test length/1 with correct and incorrect arguments.

t_length(Config) when is_list(Config) ->
    0 = length(""),
    0 = length([]),
    1 = length([1]),
    2 = length([1,a]),
    2 = length("ab"),
    3 = length("abc"),
    4 = length(id([x|"abc"])),
    6 = length("hejsan"),
    {'EXIT',{badarg,_}} = (catch length(id([a,b|c]))),
    case catch length({tuple}) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("length/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.
	      

%% Test list_to_pid/1 with correct and incorrect arguments.

t_list_to_pid(Config) when is_list(Config) ->
    Me = self(),
    MyListedPid = pid_to_list(Me),
    Me = list_to_pid(MyListedPid),
    case catch list_to_pid(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_pid/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_port/1 with correct and incorrect arguments.

t_list_to_port(Config) when is_list(Config) ->
    Me = hd(erlang:ports()),
    MyListedPid = port_to_list(Me),
    Me = list_to_port(MyListedPid),
    case catch list_to_port(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_port/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

t_list_to_ref(Config) when is_list(Config) ->
    Ref = make_ref(),
    RefStr = ref_to_list(Ref),
    Ref = list_to_ref(RefStr),
    case catch list_to_ref(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_ref/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_pid/port/ref for external pids/ports/refs.
t_list_to_ext_pidportref(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    Pid = rpc:call(Node, erlang, self, []),
    Port = hd(rpc:call(Node, erlang, ports, [])),
    Ref = rpc:call(Node, erlang, make_ref, []),

    PidStr  = pid_to_list(Pid),
    PortStr = port_to_list(Port),
    RefStr  = ref_to_list(Ref),

    Pid2  = list_to_pid(PidStr),
    Port2 = list_to_port(PortStr),
    Ref2  = list_to_ref(RefStr),

    %% Local roundtrips of externals work from OTP-23
    %% as even though 'creation' is missing in the string formats
    %% we know the 'creation' of the connected node and list_to_* use that.
    true = (Pid =:= Pid2),
    true = (Port =:= Port2),
    true = (Ref =:= Ref2),
    true = (Pid == Pid2),
    true = (Port == Port2),
    true = (Ref == Ref2),

    %% And it works when sent back to the same node instance,
    %% which was connected when list_to_* were called.
    true = rpc:call(Node, erlang, '=:=', [Pid, Pid2]),
    true = rpc:call(Node, erlang, '==',  [Pid, Pid2]),
    true = rpc:call(Node, erlang, '=:=', [Port, Port2]),
    true = rpc:call(Node, erlang, '==',  [Port, Port2]),
    true = rpc:call(Node, erlang, '=:=', [Ref, Ref2]),
    true = rpc:call(Node, erlang, '==',  [Ref, Ref2]),


    peer:stop(Peer),
    ok.

-define(NEW_PID_EXT, 88).
-define(NEW_PORT_EXT, 89).
-define(NEWER_REFERENCE_EXT, 90).

%% Copy pid/port/ref but set creation=0
make_0_creation(X) when is_pid(X); is_port(X); is_reference(X) ->
    B = term_to_binary(X),
    Sz = byte_size(B),
    B2 = case B of
             <<131, ?NEW_PID_EXT, _/binary>> ->
                 PreSz = Sz - 4,
                 <<_:PreSz/binary, Cr:32>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32>>;
             <<131, ?NEW_PORT_EXT, _/binary>> ->
                 PreSz = Sz - 4,
                 <<_:PreSz/binary, Cr:32>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32>>;
             <<131, ?NEWER_REFERENCE_EXT, Len:16, _/binary>> ->
                 PostSz = Len*4,
                 PreSz = Sz - (4 + PostSz),
                 <<_:PreSz/binary, Cr:32, PostFix:PostSz/binary>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32, PostFix/binary>>
         end,
    binary_to_term(B2).


%% Test list_to_float/1 with correct and incorrect arguments.

t_list_to_float(Config) when is_list(Config) ->
    5.89000 = list_to_float(id("5.89")),
    5.89898 = list_to_float(id("5.89898")),
    case catch list_to_float(id("58")) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("list_to_float with incorrect arg succeeded.~nResult: ~p", [Res])
    end,
    ok.

t_list_to_float_2(Config) when is_list(Config) ->
    %% Base 2
    1.25 = list_to_float("1.01", 2),
    -1.25 = list_to_float("-1.01", 2),

    %% Base 16
    3.0 = list_to_float("3.0", 16),
    1023.0 = list_to_float("3FF.0", 16),
    -1023.0 = list_to_float("-3FF.0", 16),
    255.9375 = list_to_float("FF.F", 16),

    %% Base 8
    8.5 = list_to_float("10.4", 8),

    %% Base 36
    13.375 = list_to_float("D.DI", 36),

    %% Base 10 (should match list_to_float/1)
    1.5 = list_to_float("1.5", 10),
    -1.5 = list_to_float("-1.5", 10),
    1.0e10 = list_to_float("1.0e10", 10),

    %% With exponent (#e notation)
    16.0 = list_to_float("1.0#e1", 16),
    16.0 = list_to_float("1.0#e+1", 16),
    0.0625 = list_to_float("1.0#e-1", 16),
    4.0 = list_to_float("1.0#e2", 2),
    4.0 = list_to_float("1.0#e+2", 2),

    %% With underscores
    255.9375 = list_to_float("F_F.F", 16),
    1023.0 = list_to_float("3_F_F.0", 16),

    %% Positive sign
    3.0 = list_to_float("+3.0", 16),

    %% Zero
    0.0 = list_to_float("0.0", 2),
    0.0 = list_to_float("0.0", 16),

    %% Lowercase and uppercase digits
    255.0 = list_to_float("FF.0", 16),
    255.0 = list_to_float("ff.0", 16),

    %% Invalid base
    ?assertError(badarg, list_to_float("1.0", 1)),
    ?assertError(badarg, list_to_float("1.0", 37)),

    %% Integer (no dot)
    ?assertError(badarg, list_to_float("10", 2)),
    ?assertError(badarg, list_to_float("FF", 16)),

    %% Not float
    ?assertError(badarg, list_to_float("1.2", 2)),
    ?assertError(badarg, list_to_float("G.0", 16)),
    ?assertError(badarg, list_to_float("xyz", 16)),
    ?assertError(badarg, list_to_float(atom, 16)),
    ?assertError(badarg, list_to_float("1.0", foo)),
    ?assertError(badarg, list_to_float("", 16)),

    ok.

id(I) -> I.
