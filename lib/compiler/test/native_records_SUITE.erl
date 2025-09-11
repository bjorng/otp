%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(native_records_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         local_basic/1,local_updates/1]).

-record #empty{}.
-record #a{x, y}.
-record #b{x=none, y=none, z=none}.
-record #c{x::integer, y=0::integer, z=[]}.
-record #d{f=3.1416, l=[a,b,c], t={a,b,c},
           m=#{a => 1}}.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [
       local_basic,
       local_updates
      ]}].

init_per_suite(Config) ->
    id(Config),
    %% test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

local_basic(_Config) ->
    ARec = id(#a{x=1, y=2}),
    BRec = id(#b{}),
    CRec = id(#c{x=42, y=100}),

    empty = name(id(#empty{})),
    a = name(ARec),
    b = name(BRec),
    c = name(CRec),

    NameFun = fun(#a{}) -> a;
                 (#b{}) -> b
              end,
    a = NameFun(ARec),
    b = NameFun(BRec),

    ?assertError(badarg, #b{bad_field = some_value}),

    true = is_int_ax(ARec),
    false = is_int_ax(id(#a{x=a,y=b})),

    ok.

name(#empty{}) -> empty;
name(#a{}) -> a;
name(#b{}) -> b;
name(#c{}) -> c.

is_int_ax(A) ->
    Result = is_int_ax_guard_1(A),
    Result = is_int_ax_guard_2(A),
    Result = is_integer(A#a.x),
    Result.

is_int_ax_guard_1(A) when is_integer(A#a.x) -> true;
is_int_ax_guard_1(_) -> false.

is_int_ax_guard_2(#a{}=A) when is_integer(A#a.x) -> true;
is_int_ax_guard_2(_) -> false.

local_updates(_Config) ->
    R0 = #b{},
    R1 = id(R0#b{x=foo}),
    #b{x=foo, y=none, z=none} = id(R1),
    foo = R1#b.x,
    none = R1#b.y,
    none = R1#b.z,

    R2 = id(R1#b{y=bar, z=baz}),
    #b{x=foo, y=bar, z=baz} = id(R2),
    foo = R2#b.x,
    bar = R2#b.y,
    baz = R2#b.z,

    ok.

%%% Common utilities.

id(I) ->
    I.
