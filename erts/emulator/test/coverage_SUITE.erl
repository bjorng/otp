%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
-module(coverage_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([toggle_modes/1,get_coverage/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    case erlang:system_info(coverage_support) of
        true ->
            Config;
        false ->
            {skip, "This runtime system does not support coverage"}
    end.

end_per_suite(Config) ->
    Config.

all() ->
    [toggle_modes,get_coverage].

toggle_modes(_Config) ->
    none = erlang:get_coverage_mode(?MODULE),
    OldMode = erlang:system_info(coverage_mode),
    try
        do_toggle_modes(OldMode)
    after
        erlang:system_flag(coverage_mode, OldMode)
    end.

do_toggle_modes(CurrentMode) ->
    Modes = [none,line_coverage,line_counters,function],
    Last = lists:last(Modes),
    Last = do_toggle_modes_1(CurrentMode, Modes),
    ok.

do_toggle_modes_1(Current, [Mode|Modes]) ->
    Current = erlang:system_flag(coverage_mode, Mode),
    none = erlang:get_coverage_mode(?MODULE),
    do_toggle_modes_1(Mode, Modes);
do_toggle_modes_1(Current, []) ->
    Current.

get_coverage(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    OldMode = erlang:system_info(coverage_mode),

    try
        do_get_coverage(PrivDir)
    after
        erlang:system_flag(coverage_mode, OldMode)
    end.

do_get_coverage(PrivDir) ->
    M = get_coverage_test,
    S = ~"""
        -module(get_coverage_test).
        -export([fact/1,fib/1]).

        fact(N) when is_integer(N), N >= 0 ->   %4
            fact(N, 1).                         %5

        fact(0, P) ->
            P;                                  %8
        fact(N, P) ->
            fact(N - 1, P * N).                 %10

        fib(N) ->
            fib(N, 0, 1).                       %13

        fib(0, _, B) ->
            B;                                  %16
        fib(N, A, B) ->
            fib(N - 1, B, A + B).               %18
        """,

    ErlFile = filename:join(PrivDir, atom_to_list(M) ++ ".erl"),
    ok = file:write_file(ErlFile, S),
    {ok,M,Beam} = compile:file(ErlFile, [report,binary,line_coverage]),

    Run1 = fun() -> ok end,
    Result1 = {[],
               [{5,0},{8,0},{10,0},{13,0},{16,0},{18,0}]},
    do_get_coverage(M, Beam, Run1, Result1),

    Run2 = fun() -> M:fib(5) end,
    Result2 = {[{{fib,1},true}, {{fib,3},true}],
               [{5,0},{8,0},{10,0},{13,1},{16,1},{18,5}]},
    do_get_coverage(M, Beam, Run2, Result2),

    {ok,M,BeamFun} = compile:file(ErlFile, [report,binary]),
    do_get_function_coverage(M, BeamFun, Run1, Result1),

    none = erlang:get_coverage_mode(?MODULE),

    ok.

do_get_coverage(M, Beam, RunFun, Result) ->
    {FunctionResult,LineCoverage} = Result,

    %% Test function coverage.

    do_get_function_coverage(M, Beam, RunFun, Result),

    %% Test line_coverage.

    _ = erlang:system_flag(coverage_mode, line_coverage),
    {module,M} = code:load_binary(M, "", Beam),
    _ = erlang:system_flag(coverage_mode, none),
    RunFun(),

    line_coverage = erlang:get_coverage_mode(M),

    LineCoverageBool = [{F,N =/= 0} || {F,N} <- LineCoverage],
    FunctionResult = get_function_coverage(M),
    LineCoverageBool = erlang:get_line_coverage(M),

    LineCoverageBoolReset = [{F,false} || {F,_} <- LineCoverage],
    erlang:reset_coverage(M),
    LineCoverageBoolReset = erlang:get_line_coverage(M),

    unload(M),

    %% Test line_counters.

    _ = erlang:system_flag(coverage_mode, line_counters),
    {module,M} = code:load_binary(M, "", Beam),
    _ = erlang:system_flag(coverage_mode, none),
    RunFun(),

    line_counters = erlang:get_coverage_mode(M),

    FunctionResult = get_function_coverage(M),
    LineCoverage = erlang:get_line_coverage(M),

    LineCoverageZero = [{F,0} || {F,_} <- LineCoverage],
    erlang:reset_coverage(M),
    LineCoverageZero = erlang:get_line_coverage(M),

    unload(M),

    {'EXIT',{badarg,_}} = catch get_function_coverage(M),
    {'EXIT',{badarg,_}} = catch erlang:get_line_coverage(M),
    {'EXIT',{badarg,_}} = catch erlang:get_coverage_mode(M),

    ok.

do_get_function_coverage(M, Beam, RunFun, Result) ->
    {FunctionResult,_LineCoverage} = Result,

    %% Test function coverage.

    _ = erlang:system_flag(coverage_mode, function),
    {module,M} = code:load_binary(M, "", Beam),
    _ = erlang:system_flag(coverage_mode, none),
    RunFun(),

    function = erlang:get_coverage_mode(M),

    FunctionResult = get_function_coverage(M),
    {'EXIT',{badarg,_}} = catch erlang:get_line_coverage(M),

    erlang:reset_coverage(M),
    [] = get_function_coverage(M),

    unload(M),

    {'EXIT',{badarg,_}} = catch erlang:get_coverage_mode(M),
    {'EXIT',{badarg,_}} = catch get_function_coverage(M),
    {'EXIT',{badarg,_}} = catch erlang:get_line_coverage(M),

    ok.

get_function_coverage(M) ->
    Covered = erlang:get_function_coverage(M),
    AllFunctions = sets:from_list(M:module_info(functions)),

    %% Ensure that the module info functions are not present.
    false = lists:keyfind({module_info,0}, 1, Covered),
    false = lists:keyfind({module_info,1}, 1, Covered),

    %% Remove functions that are not covered and actually exist in the
    %% module.
    [{F,Bool} ||
        {F,Bool} <- Covered,
        Bool =:= true orelse not sets:is_element(F, AllFunctions)].

unload(M) ->
    true = code:delete(M),
    _ = code:purge(M),
    ok.
