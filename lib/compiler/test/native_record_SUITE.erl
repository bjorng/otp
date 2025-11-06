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
         any_record/1,is_record_bif/1,
         get_field_names_bif/1]).

-record #empty{}.
-record #a{x, y}.
-record #b{x=none, y=none, z=none}.
-record #c{x::integer, y=0::integer, z=[]}.
-record #d{f=3.1416, l=[a,b,c], t={a,b,c},
           m=#{a => 1}}.
-record #e{x=0.0}.

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
       any_record,
       is_record_bif,
       get_field_names_bif
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

    ~"ARec: #native_record_SUITE:a{x = 1, y = 2}\n" =
        iolist_to_binary(io_lib:format("ARec: ~p~n", [ARec])),
    ~"BRec: #native_record_SUITE:b{x = none, y = none, z = none}\n" =
        iolist_to_binary(io_lib:format("BRec: ~w~n", [BRec])),
    ~"CRec: #native_record_SUITE:c{x = 42, y = 100, z = []}\n" =
        iolist_to_binary(io_lib:format("CRec: ~p~n", [CRec])),

    empty = name(id(#empty{})),
    a = name(ARec),
    b = name(BRec),
    c = name(CRec),

    NameFun = fun(#a{}) -> a;
                 (#b{}) -> b
              end,
    a = NameFun(ARec),
    b = NameFun(BRec),

    ?assertError({badfield,foobar}, #b{foobar = some_value}),

    ?assertError({badrecord,b}, ARec#b{x=99}),
    ?assertError({badfield,bad_field}, BRec#b{bad_field = some_value}),

    ?assertError({novalue,x}, #a{}),
    ?assertError({novalue,y}, #a{x=1}),
    ?assertError({novalue,x}, #a{y=1}),

    true = is_int_ax(ARec),
    false = is_int_ax(id(#a{x=a,y=b})),

    try id(throw(ARec)) of
        _ ->
            error(should_fail)
    catch
        throw:#a{x=1, y=2} ->
            ok
    end,

    %% Test garbage collection.
    N = 10000,
    Seq = lists:seq(1, N),
    RecList = [#a{x=I,y=I*I} || I <- Seq],
    [] = [{I,R#a.x,R#a.y} || I <- Seq && R <- RecList,
                             not(R#a.x =:= I andalso R#a.y =:= I*I)],

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

    N = 10000,
    #a{x=N,y=0} =
        lists:foldl(fun(_, #a{x=X,y=Y}=R) ->
                            R#a{x=X+1,y=Y-1}
                    end, #a{x=0,y=N}, lists:seq(1, N)),

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

any_record(_Config) ->
    {777,888} = get_any_xy(#a{x=777,y=888}),
    {77,88} = get_any_xy(#Point{x=77,y=88}),
    none = get_any_xy(#div{}),

    ARec0 = id(#a{x=1,y=0}),
    CRec0 = id(#c{x=1,y=0,z=[]}),

    #a{x=7,y=13} = ARec = update_any_xy(ARec0, 7, 13),
    #_{x=7,y=13} = ARec,

    #c{x=100,y=200} = CRec = update_any_xy(CRec0, 100, 200),
    #_{x=100,y=200} = CRec,

    ?assertError({badfield,x}, update_any_xy(id(#d{}), 0, 0)),
    ?assertError({badfield,y}, update_any_xy(id(#e{}), 0, 0)),

    ok.

get_any_xy(#_{x=X,y=Y}=R) ->
    X = R#_.x,
    Y = R#_.y,
    {X,Y};
get_any_xy(_) ->
    none.

update_any_xy(R, X, Y) ->
    R#_{x=X,y=Y}.

is_record_bif(Config) ->
    false = is_record(Config, #empty),
    false = is_record(Config, #?MODULE:empty),
    false = is_record(Config, #a),
    false = is_record(Config, #?MODULE:a),

    BR = id(#b{}),
    true = is_record(BR, #b),
    true = is_record(BR, #?MODULE:b),

    ok.

get_field_names_bif(_Config) ->
    ARec = id(#a{x=1, y=2}),
    BRec = id(#b{}),
    CRec = id(#c{x=42, y=100}),

    ARec = records:create(?MODULE, a, #{x=>1, y=>2}),
    BRec = records:create(?MODULE, b, #{}),
    CRec = records:create(?MODULE, c, #{x=>42, y=>100}),

    [x,y] = records:get_field_names(ARec),
    [x,y,z] = records:get_field_names(BRec),
    [x,y,z] = records:get_field_names(CRec),

    R0 = #b{},
    R1 = records:update(R0, ?MODULE, b, #{x=>foo}),
    #b{x=foo, y=none, z=none} = id(R1),
    #?MODULE:b{x=foo, y=none, z=none} = id(R1),

    N = 10000,
    #a{x=N,y=0} =
        lists:foldl(fun(_, #a{x=X,y=Y}=R) ->
                            records:update(R, ?MODULE, a, #{x=>X+1,y=>Y-1})
                    end, #a{x=0,y=N}, lists:seq(1, N)),
    ok.

%%% Common utilities.

id(I) ->
    I.
