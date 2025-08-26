%%
%% %CopyrightBegin%
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
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the structs behaviour
%%% (when using short names/imports).
%%%-----------------------------------------------------------------

-module(structs_i_SUITE).

-import_struct(a, [a, b, c]).

-export([all/0, suite/0, groups/0]).

-export([init_per_group/2, end_per_group/2]).

%% tests
-export([t_1/1, t_2/1, t_3/1, t_4/1, t_6/1, t_7/1, t_8/1, t_9/1, t_10/1,
         t_11/1, t_12/1, t_13/1, t_14/1, t_15/1, t_16/1, t_17/1, t_18/1, t_19/1, t_20/1]).
-export([id/1]).

suite() ->
  [{ct_hooks,[ts_install_cth]},
    {timetrap,{minutes,1}}].

all() ->
  [{group, simple}].

groups() ->
  [{simple, [t_1, t_2, t_3, t_4, t_6, t_7, t_8, t_9, t_10,
             t_11, t_12, t_13, t_14, t_15, t_16, t_17, t_18, t_19, t_20]}].

init_definitions() ->
  struct:define(a,a,
    {{a1,a1_default},
      {a2,a2_default}}
  ),
  struct:define(a,b,
    {{b1,b1_default},
      {b2,b2_default}}
  ),
  struct:define(a,c,
    {{a1,a1_default},
      {a2,a2_default}}
  ).

%%%%%%%%%%%%%

name(#a{}) -> a;
name(#b{}) -> b.

t_1(_Config) ->
  a = name(#a{}),
  b = name(#b{}),
  NameFun = fun (#a{}) -> a; (#b{}) -> b end,
  a = NameFun(#a{}),
  b = NameFun(#b{}).

%%%%%%%%%%%%%

get_a1(#a{a1 = A1}) -> A1.

t_2(_Config) ->
  Str1 = #a{},
  Str2 = #a{a1 = non_default},
  a1_default = get_a1(Str1),
  non_default = get_a1(Str2).

% struct creation with bad field
t_3(_Config) ->
  BadFieldRes =
  try
    _Str = #a{bad_field = non_default},
     no_fail
  catch
    _:_ -> fail
  end,
  fail = BadFieldRes.

% pattern matching
t_4(_Config) ->
  Str = #a{},
  #a{} = Str,
  #a{a1 = A1} = Str,
  a1_default = A1.

get_as(#a{a1 = A1, a2 = A2}) ->
  {A1, A2}.

t_6(_Config) ->
  Str = #a{},
  As = get_as(Str),
  {a1_default, a2_default} = As,
  ok,
  Str1 = #a{a1 = structs_SUITE:id(a1), a2 = structs_SUITE:id(a2)},
  As1 = get_as(Str1),
  {a1, a2} = As1,
  ok.

%%
t_7(_Config) ->
  F1 = fun (#a{a3 = A3}) -> A3; (#a{a1 = A1}) -> A1 end,
  F2 = fun (#a{a1 = #a{}}) -> a1; (#a{a1 = A1}) -> A1 end,
  a1_default = F1(#a{}),
  a1_default = F2(#a{}).

%% updates
t_8(_Config) ->
  Str = #a{a1 = a10, a2 = a20},
  Str1 = Str#a{a1 = a11, a2 = a21},
  a11 = Str1#a.a1,
  a21 = Str1#a.a2,
  ok.

%% maps/hashes
t_9(_Config) ->
  Str1 = #a{},
  Str2 = #b{},
  M = #{Str1 => a, Str2 => b},
  a = maps:get(Str1, M),
  b = maps:get(Str2, M),
  ok.

t_10(_Config) ->
  Str1 = #a{},
  Str2 = #b{},
  M = #{#a{} => a, #b{} => b, #a{a1 = a1, a2 = a2} => a1, #b{b1 = b1, b2 = b2} => b1},
  a = maps:get(Str1, M),
  b = maps:get(Str2, M),
  a1 = maps:get(#a{a1 = a1, a2 = a2}, M),
  b1 = maps:get(#b{b1 = b1, b2 = b2}, M),
  ok.

match1(#a{a1 = a1}, #b{b1 = b1}) ->
  f1;
match1(#a{a2 = a2}, #b{b2 = b2}) ->
  f2;
match1(#{a1 := #a{a1 = #b{b2 = #{a1 := #a{a1 = A1}}}}}, _) ->
  A1;
match1(_, _) ->
  default.

t_11(_Config) ->
  default = match1(id(1), id(2)),
  f1 = match1(id(#a{a1 = a1, a2 = a3}), #b{b1 = b1}),
  f2 = match1(id(#a{a1 = a2, a2 = a2}), #b{b1 = b1, b2 = b2}),
  42 = match1(id(#{a1 => #a{a1 = #b{b2 = #{a1 => #a{a1 = 42}}}}}), ignore),
  ok.

is_aa1(X) when X#a.a1 == 1 ->
  a1;
is_aa1(_) ->
  no.

is_aa12(X) when X#_.a1 == 1 ->
  a1;
is_aa12(_) ->
  no.

t_12(_Config) ->
  Str1 = #a{a1 = 1},
  Str2 = #a{},
  a1 = is_aa1(Str1),
  a1 = is_aa12(Str1),
  no = is_aa1(Str2),
  no = is_aa12(Str2),
  no = is_aa1([]),
  no = is_aa12([]).

t_13(_Config) ->
  Str1 = #a{a1 = 1},
  Str2 = #c{a1 = 1},
  a1 = is_aa1(Str1),
  no = is_aa1(Str2).

is_a_call(X) when struct_name(X) == a ->
  yes;
is_a_call(_) ->
  no.

t_14(_Config) ->
  Str1 = #a{a1 = 1},
  Str2 = #c{a1 = 1},
  yes = is_a_call(Str1),
  no = is_a_call(Str2).

t_15(_Config) ->
  % testing complex X&a.field in guards
  A = #a{a1 = a},
  B = #b{b1 = a},
  C = #c{},
  F1 =  fun (X) when X#a.a1 == a; X#b.b1 == a -> true; (_) -> false end,
  true = F1(A),
  true = F1(B),
  false = F1(C),
  F2 =  fun (X) when X#a.a1 == a -> a; (X) when X#b.b1 == a -> b; (_) -> other end,
  a = F2(A),
  b = F2(B),
  other = F2(C).

t_16(_Config) ->
  A = #a{a1 = a},
  B = #b{b1 = a},
  C = #c{},
  ABC = [A, B, C],
  [A] = [X || X <- ABC, X#a.a1 == a].

t_17(_Config) ->
  A1 = #a{a1 = a},
  A2 = #a{a1 = b},
  AA = [A1, A2],
  [A1] = [X || X <- AA, id(X#a.a1 == a)].

t_18(_Config) ->
  A = #a{a1 = a},
  B = #b{},
  AB = [A, B],
  fail = try
    % fails with badstruct
    [A] = [X || X <- AB, id(X#a.a1 == a)]
  catch _:_ -> fail end.

get_any_a1(#_{a1 = A}) -> {ok, A};
get_any_a1(_) -> no.

get_any_a2(S) ->
  S#_.a1.

get_any_a3(S) ->
  S#_.a1.

t_19(_Config) ->
  A = #a{a1 = a},
  {ok, a} = get_any_a1(A),
  no = get_any_a1({}),
  a = get_any_a2(A),
  a = get_any_a3(A).

t_20(_Config) ->
  A = #a{},
  A1 = A#_{a1 = a1, a2 = a2},
  #_{a1 = a1} = A1.


init_per_group(_, Config) ->
  %% this is a hack for now
  init_definitions(),
  Config.

end_per_group(_, Config) ->
  Config.

id(X) -> X.