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
-module(native_record_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         local_basic/1,local_updates/1,non_atomic_names/1,
         matching_any_record/1]).

-record #empty{}.
-record #a{x, y}.
-record #b{x=none, y=none, z=none}.
-record #c{x::integer, y=0::integer, z=[]}.
-record #d{f=3.1416, l=[a,b,c], t={a,b,c},
           m=#{a => 1}}.

%% Records with non-atomic names.
-record #div{attr=0}.
-record #rem{n=0}.
-record #Seq{elements=[]}.
-record #Point{x=0,y=0,z=0}.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [
       local_basic,
       local_updates,
       non_atomic_names,
       matching_any_record
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

    try id(throw(ARec)) of
        _ ->
            error(should_fail)
    catch
        throw:#a{x=1, y=2} ->
            ok
    end,

    %% Cover v3_core:sanitize/1.
    ?assertError({badmatch, ARec}, (#a{}=[_]) = id(ARec)),

    ok.

name(#empty{}) -> empty;
name(#a{}) -> a;
name(#b{}) -> b;
name(#c{}) -> c;
name(#div{}) -> 'div';
name(#Seq{}) -> 'Seq'.

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
    #?MODULE:b{x=foo, y=none, z=none} = id(R1),
    foo = R1#b.x,
    none = R1#b.y,
    none = R1#b.z,

    R2 = id(R1#b{y=bar, z=baz}),
    R2 = id(R1#?MODULE:b{y=bar, z=baz}),
    #b{x=foo, y=bar, z=baz} = id(R2),
    foo = R2#b.x,
    bar = R2#b.y,
    baz = R2#b.z,

    foo = R2#?MODULE:b.x,
    bar = R2#?MODULE:b.y,
    baz = R2#?MODULE:b.z,

    ok.

non_atomic_names(_Config) ->
    Div0 = id(#div{attr=42}),
    Seq0 = id(#Seq{elements=[1,2,3]}),

    'div' = name(Div0),
    'Seq' = name(Seq0),

    Div0 = id(#?MODULE:div{attr=42}),
    Seq0 = id(#?MODULE:Seq{elements=[1,2,3]}),

    42 = Div0#div.attr,
    42 = Div0#?MODULE:div.attr,

    [1,2,3] = Seq0#Seq.elements,
    [1,2,3] = Seq0#?MODULE:Seq.elements,

    Div = id(Div0#div{attr=99}),
    Seq = id(Seq0#Seq{elements=[10]}),

    Div = id(Div0#?MODULE:div{attr=99}),
    Seq = id(Seq0#?MODULE:Seq{elements=[10]}),

    99 = non_atomic_names_match(Div),
    [10] = non_atomic_names_match(Seq),
    100 = non_atomic_names_match(#rem{n=100}),

    Point = #Point{x=13,y=40,z=1},
    Point = #Point{x=13,y=40,z=1} = non_atomic_names_match(Point),

    ARecDiv0 = id(#a{x=#div{attr=7}, y=#div{attr=9}}),
    7 = ARecDiv0#a.x#div.attr,
    #div{attr=13} = id(ARecDiv0#a.x#div{attr=13}),

    DivRem0 = id(#div{attr=#rem{n=42}}),
    42 = DivRem0#div.attr#rem.n,

    DivRem = id(#?MODULE:div{attr=#?MODULE:rem{n=77}}),
    77 = DivRem#div.attr#rem.n,

    try id(throw(Div)) of
        _ ->
            error(should_fail)
    catch
        throw:#div{attr=99} ->
            ok
    end,

    try id(error(Seq)) of
        _ ->
            error(should_fail)
    catch
        error:#?MODULE:Seq{elements=[10]} ->
            ok
    end,

    ok.

non_atomic_names_match(R) ->
    case R of
        #div{attr=Attr} ->
            Attr;
        #?MODULE:Seq{elements=Es} ->
            Es;
        #?MODULE:rem{n=N} ->
            N;
        #Point{x=X,y=Y,z=Z}=Point when X =/= 0, Y =/= 0, Z =/= 0 ->
            Point
    end.

matching_any_record(_Config) ->
    {777,888} = get_any_xy(#a{x=777,y=888}),
    {77,88} = get_any_xy(#Point{x=77,y=88}),
    none = get_any_xy(#div{}),
    ok.

get_any_xy(#_{x=X,y=Y}=R) ->
    X = R#_.x,
    Y = R#_.y,
    {X,Y};
get_any_xy(_) ->
    none.

%%% Common utilities.

id(I) ->
    I.
