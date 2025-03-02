%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-module(ordsets).
-moduledoc """
Functions for manipulating sets as ordered lists.

Sets are collections of elements with no duplicate elements. An `ordset` is a
representation of a set, where an ordered list is used to store the elements of
the set. An ordered list is more efficient than an unordered list. Elements are
ordered according to the _Erlang term order_.

This module provides the same interface as the `m:sets` module but with a
defined representation. One difference is that while `sets` considers two
elements as different if they do not match (`=:=`), this module considers two
elements as different if and only if they do not compare equal (`==`).

See the [Compatibility Section in the `sets` module](`m:sets#module-compatibility`)
for more information about the compatibility of the different implementations of
sets in the Standard Library.

## See Also

`m:gb_sets`, `m:sets`
""".

-export([new/0,is_set/1,size/1,is_empty/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_equal/2, is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2,map/2,filtermap/2]).

-export_type([ordset/1]).

-doc "As returned by `new/0`.".
-type ordset(T) :: [T].

-doc """
Returns a new empty ordered set.

## Examples

```erlang
> ordsets:new()
[]
```
""".
-spec new() -> [].

new() -> [].

-doc """
Returns `true` if `Ordset` is an ordered set of elements; otherwise,
returns `false`.

This function returns true for any ordered list, even if it was not
constructed using the functions in this module.

## Examples

```erlang
> ordsets:is_set(ordsets:from_list([a,x,13,{p,q}])).
true
> ordsets:is_set([a,b,c]).
true
> ordsets:is_set([z,a]).
false
> ordsets:is_set({a,b}).
false
```
""".
-spec is_set(Ordset) -> boolean() when
      Ordset :: term().

is_set([E|Es]) -> is_set(Es, E);
is_set([]) -> true;
is_set(_) -> false.

is_set([E2|Es], E1) when E1 < E2 ->
    is_set(Es, E2);
is_set([_|_], _) -> false;
is_set([], _) -> true.

-doc """
Returns the number of elements in `Ordset`.

## Examples

```erlang
> ordsets:size(ordsets:new()).
0
> ordsets:size(ordsets:from_list([1,2,3])).
3
```
""".
-spec size(Ordset) -> non_neg_integer() when
      Ordset :: ordset(_).

size(S) -> length(S).

-doc """
Returns `true` if `Ordset` is an empty set; otherwise, returns `false`.

## Examples

```erlang
> ordsets:is_empty(ordsets:new()).
true
> ordsets:is_empty(ordsets:from_list([1])).
false
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec is_empty(Ordset) -> boolean() when
      Ordset :: ordset(_).

is_empty(S) -> S =:= [].

-doc """
Returns `true` if `Ordset1` and `Ordset2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
> Empty = ordsets:new().
> S = ordsets:from_list([a,b]).
> ordsets:is_equal(S, S)
true
> ordsets:is_equal(S, Empty)
false
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec is_equal(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_equal(S1, S2) when is_list(S1), is_list(S2) ->
    S1 == S2.

-doc """
Returns the elements of `Ordset` as a list.

## Examples

```erlang
> S = ordsets:from_list([a,b]).
> ordsets:to_list(S).
[a,b]
```
""".
-spec to_list(Ordset) -> List when
      Ordset :: ordset(T),
      List :: [T].

to_list(S) -> S.

-doc """
Returns an ordered set of the elements in `List`.

## Examples

```erlang
> ordsets:from_list([a,b,a,b,b,c]).
[a,b,c]
```
""".
-spec from_list(List) -> Ordset when
      List :: [T],
      Ordset :: ordset(T).

from_list(L) ->
    lists:usort(L).

-doc """
Returns `true` if `Element` is an element of `Ordset`; otherwise, returns `false`.

## Examples

```erlang
> S = ordsets:from_list([a,b,c]).
> ordsets:is_element(42, S).
false
> ordsets:is_element(b, S).
true
```
""".
-spec is_element(Element, Ordset) -> boolean() when
      Element :: term(),
      Ordset :: ordset(_).

is_element(E, [H|Es]) when E > H -> is_element(E, Es);
is_element(E, [H|_]) when E < H -> false;
is_element(_E, [_H|_]) -> true;			%E == H
is_element(_, []) -> false.

-doc """
Returns a new ordered set formed from `Ordset1` with `Element` inserted.

## Examples

```erlang
> S0 = ordsets:new().
[]
> S1 = ordsets:add_element(7, S0).
[7]
> S2 = ordsets:add_element(42, S1).
[7,42]
> ordsets:add_element(42, S2).
[7,42]
```
""".
-spec add_element(Element, Ordset1) -> Ordset2 when
      Element :: E,
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T | E).

add_element(E, [H|Es]) when E > H -> [H|add_element(E, Es)];
add_element(E, [H|_]=Set) when E < H -> [E|Set];
add_element(_E, [_H|_]=Set) -> Set;		%E == H
add_element(E, []) -> [E].

-doc """
Returns a copy of `Ordset1` with `Element` removed.

## Examples

```erlang
> S = ordsets:from_list([a,b,c]).
> ordsets:del_element(c, S).
[a,b]
> ordsets:del_element(x, S).
[a,b,c]
```
""".
-spec del_element(Element, Ordset1) -> Ordset2 when
      Element :: term(),
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T).

del_element(E, [H|Es]) when E > H -> [H|del_element(E, Es)];
del_element(E, [H|_]=Set) when E < H -> Set;
del_element(_E, [_H|Es]) -> Es;			%E == H
del_element(_, []) -> [].

-doc """
Returns the union of `Ordset1` and `Ordset2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
> S0 = ordsets:from_list([a,b,c,d]).
> S1 = ordsets:from_list([c,d,e,f]).
> ordsets:union(S0, S1).
[a,b,c,d,e,f]
```
""".
-spec union(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(T1),
      Ordset2 :: ordset(T2),
      Ordset3 :: ordset(T1 | T2).

union([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|union(Es1, Set2)];
union([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    [E2|union(Es2, Set1)];			% switch arguments!
union([E1|Es1], [_E2|Es2]) ->			%E1 == E2
    [E1|union(Es1, Es2)];
union([], Es2) -> Es2;
union(Es1, []) -> Es1.

-doc """
Returns the merged (union) set of the list of sets.

## Examples

```erlang
> S0 = ordsets:from_list([a,b,c,d]).
> S1 = ordsets:from_list([d,e,f]).
> S2 = ordsets:from_list([q,r])
> Sets = [S0, S1, S2].
> ordsets:union(Sets).
[a,b,c,d,e,f,q,r]
```
""".
-spec union(OrdsetList) -> Ordset when
      OrdsetList :: [ordset(T)],
      Ordset :: ordset(T).

union(OrdsetList) ->
    lists:umerge(OrdsetList).

%% intersection(OrdSet1, OrdSet2) -> OrdSet.
%%  Return the intersection of OrdSet1 and OrdSet2.

-doc "Returns the intersection of `Ordset1` and `Ordset2`.".
-spec intersection(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_),
      Ordset3 :: ordset(_).

intersection([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    intersection(Es1, Set2);
intersection([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    intersection(Es2, Set1);			% switch arguments!
intersection([E1|Es1], [_E2|Es2]) ->		%E1 == E2
    [E1|intersection(Es1, Es2)];
intersection([], _) ->
    [];
intersection(_, []) ->
    [].

%% intersection([OrdSet]) -> OrdSet.
%%  Return the intersection of the list of ordered sets.

-doc "Returns the intersection of the non-empty list of sets.".
-spec intersection(OrdsetList) -> Ordset when
      OrdsetList :: [ordset(_),...],
      Ordset :: ordset(_).

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(OrdSet1, OrdSet2) -> boolean().
%%  Check whether OrdSet1 and OrdSet2 are disjoint.

-doc """
Returns `true` if `Ordset1` and `Ordset2` are disjoint (have no elements in
common), otherwise `false`.
""".
-spec is_disjoint(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_disjoint([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    is_disjoint(Es1, Set2);
is_disjoint([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_disjoint(Es2, Set1);			% switch arguments!
is_disjoint([_E1|_Es1], [_E2|_Es2]) ->		%E1 == E2
    false;
is_disjoint([], _) ->
    true;
is_disjoint(_, []) ->
    true.

%% subtract(OrdSet1, OrdSet2) -> OrdSet.
%%  Return all and only the elements of OrdSet1 which are not also in
%%  OrdSet2.

-doc "Returns only the elements of `Ordset1` that are not also elements of `Ordset2`.".
-spec subtract(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_),
      Ordset3 :: ordset(_).

subtract([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|subtract(Es1, Set2)];
subtract([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    subtract(Set1, Es2);
subtract([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    subtract(Es1, Es2);
subtract([], _) -> [];
subtract(Es1, []) -> Es1.

%% is_subset(OrdSet1, OrdSet2) -> boolean().
%%  Return 'true' when every element of OrdSet1 is also a member of
%%  OrdSet2, else 'false'.

-doc """
Returns `true` when every element of `Ordset1` is also a member of `Ordset2`,
otherwise `false`.
""".
-spec is_subset(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_subset([E1|_], [E2|_]) when E1 < E2 ->	%E1 not in Set2
    false;
is_subset([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_subset(Set1, Es2);
is_subset([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    is_subset(Es1, Es2);
is_subset([], _) -> true;
is_subset(_, []) -> false.

%% fold(Fun, Accumulator, OrdSet) -> Accumulator.
%%  Fold function Fun over all elements in OrdSet and return Accumulator.

-doc """
Folds `Function` over every element in `Ordset` and returns the final value of
the accumulator.
""".
-spec fold(Function, Acc0, Ordset) -> Acc1 when
      Function :: fun((Element :: T, AccIn :: term()) -> AccOut :: term()),
      Ordset :: ordset(T),
      Acc0 :: term(),
      Acc1 :: term().

fold(F, Acc, Set) ->
    lists:foldl(F, Acc, Set).

%% filter(Fun, OrdSet) -> OrdSet.
%%  Filter OrdSet with Fun.

-doc "Filters elements in `Ordset1` with boolean function `Pred`.".
-spec filter(Pred, Ordset1) -> Ordset2 when
      Pred :: fun((Element :: T) -> boolean()),
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T).

filter(F, Set) ->
    lists:filter(F, Set).

%% map(Fun, OrdSet) -> OrdSet.
%%  Map OrdSet with Fun.

-doc "Maps elements in `Ordset1` with mapping function `Fun`.".
-doc(#{since => <<"OTP 27.0">>}).
-spec map(Fun, Ordset1) -> Ordset2 when
    Fun :: fun((Element1 :: T1) -> Element2 :: T2),
    Ordset1 :: ordset(T1),
    Ordset2 :: ordset(T2).

map(F, Set) ->
    from_list(lists:map(F, Set)).

%% filtermap(Fun, OrdSet) -> OrdSet.
%%  Filter and map Ordset with Fun.
-doc "Filters and maps elements in `Ordset1` with function `Fun`.".
-doc(#{since => <<"OTP 27.0">>}).
-spec filtermap(Fun, Ordset1) -> Ordset2 when
      Fun :: fun((Element1 :: T1) -> boolean | ({true, Element2 :: T2})),
      Ordset1 :: ordset(T1),
      Ordset2 :: ordset(T1 | T2).

filtermap(F, Set) ->
    from_list(lists:filtermap(F, Set)).
