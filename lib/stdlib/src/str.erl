%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% A string library that works on grapheme clusters, with the exception
%% of codepoints of class 'prepend' and non modern (or decomposed) Hangul.
%% If these codepoints appear, functions like 'find/2' may return a string
%% which starts inside a grapheme cluster.
%% These exceptions are made because the codepoints classes are
%% seldom used and require that we are able look at previous codepoints in
%% the stream and is thus hard to implement effectively.
%%
%% GC (grapheme cluster) means that searching for [101,778] 'e̊' in the
%% middle of a string of 'e's should find the 'e̊' not an 'e'
%%
%% find("eeeee̊eee", "e̊") -> "e̊ee".
%%
%% And the other way around searching for 'e' in a string the contains 'e̊'
%% should find the 'e' not the 'e̊':
%% find("1£4e̊abcdef", "e") -> "ef"
%%
%% Most functions expect all input to be normalized to one form,
%% see unicode:characters_to_nfc and unicode:characters_to_nfd functions.
%% When appending strings no checking is done to verify that the
%% result is valid unicode strings.
%%
%% Return value should be kept consistent when return type is unicode:chardata()
%% i.e. binary input => binary output, list input => list output
%%      mixed input => mixed output
%%
-module(str).

-export([is_empty/1, length/1, to_graphemes/1,
         concat/2, reverse/1,
         equal/2, equal/3, equal/4,
         slice/2, slice/3, join/2,
         pad/2, pad/3, pad/4, strip/1, strip/2, strip/3, chomp/1,
         tokens/2,
         uppercase/1, lowercase/1, titlecase/1,casefold/1,
         prefix/2,
         split/2,split/3,replace/3,replace/4,
         find/2,find/3,
         cp/1, gc/1
        ]).

-compile({no_auto_import,[length/1]}).

-export_type([grapheme_cluster/0]).

-type grapheme_cluster() :: char() | [char()].
-type direction() :: 'leading' | 'trailing'.

%% Check if string is the empty string
-spec is_empty(unicode:chardata()) -> boolean().
is_empty([]) -> true;
is_empty(<<>>) -> true;
is_empty([L|R]) -> is_empty(L) andalso is_empty(R);
is_empty(_) -> false.

%% Count the number of grapheme clusters in chardata
-spec length(unicode:chardata()) -> non_neg_integer().
length(CD) ->
    length_1(unicode_util:gc(CD), 0).

%% Convert a string to a list of grapheme clusters
-spec to_graphemes(unicode:chardata()) -> [grapheme_cluster()].
to_graphemes(CD0) ->
    case unicode_util:gc(CD0) of
        [GC|CD] -> [GC|to_graphemes(CD)];
        [] -> []
    end.

%% Compare two strings return boolean, assumes that the input are
%% normalized to same form, see unicode:characters_to_nfX_xxx(..)
-spec equal(A::unicode:chardata(), B::unicode:chardata()) -> boolean().
equal(A, A) when is_list(A); is_binary(A) ->
    true;
equal(A,B) when is_binary(A), is_binary(B) ->
    false;
equal(A,B) ->
    case equal_1(A,B) of
        false -> false;
        [] -> true;
        <<>> -> true;
        _ -> false
    end.

%% Compare two strings return boolean, assumes that the input are
%% normalized to same form, see unicode:characters_to_nfX_xxx(..)
%% does casefold on the fly
-spec equal(A::unicode:chardata(), B::unicode:chardata(), IgnoreCase::boolean()) -> boolean().
equal(A, B, false) ->
    equal(A,B);
equal(A, B, true) ->
    equal_nocase(A,B).

%% Compare two strings return boolean
%% if specified does casefold and normalization on the fly
-spec equal(A, B, IgnoreCase, Norm) -> boolean() when
      A :: unicode:chardata(),
      B :: unicode:chardata(),
      IgnoreCase :: boolean(),
      Norm :: 'none' | 'nfc' | 'nfd' | 'nfkc' | 'nfkd'.
equal(A, B, Case, none) ->
    equal(A,B,Case);
equal(A, B, false, Norm) ->
    equal_norm(A, B, Norm);
equal(A, B, true, Norm) ->
    equal_norm_nocase(A, B, Norm).

%% Reverse grapheme clusters
-spec reverse(unicode:chardata()) -> [grapheme_cluster()].
reverse(CD) ->
    reverse_1(CD, []).

%% Append a character or string to a strings.
%% Normally we just append with Str = [Str1|Str2] or [Str1,Str2]
%% But if we want to keep a certain type we can use concat which copies the data.
-spec concat(A::grapheme_cluster()|unicode:chardata(),B::unicode:chardata()) -> unicode:chardata().
concat([_|_]=A, []) -> A;
concat([], B) when is_list(B); is_binary(B) -> B;
concat(<<>>, B) when is_list(B); is_binary(B) -> B;
concat(A, <<>>) when is_list(A); is_binary(A) -> A;
concat(A, B) when is_binary(A),is_binary(B) -> <<A/binary,B/binary>>;
concat(A, B) when is_list(A), is_list(B) -> A++B;
concat(A, B) when is_integer(A), is_list(B) -> [A|B];
concat(A, B) when is_integer(A), is_binary(B) -> <<A/utf8,B/binary>>;
concat(A, B) when is_list(B); is_binary(B) -> [A|B].

%% Slice a string and return rest of string
%% Note: counts grapheme_clusters
-spec slice(unicode:chardata(), Start) -> Slice when
      Start :: non_neg_integer(),
      Slice :: unicode:chardata().
slice(CD, N) when is_integer(N), N >= 0 ->
    slice_l(CD, N, is_binary(CD)).

-spec slice(unicode:chardata(), Start, Length) -> Slice when
      Start :: non_neg_integer(),
      Length :: 'infinity' | non_neg_integer(),
      Slice :: unicode:chardata().
slice(CD, N, Length) when is_integer(N), N >= 0, is_integer(Length), Length > 0 ->
    slice_trail(slice_l(CD, N, is_binary(CD)), Length);
slice(CD, N, infinity) ->
    slice_l(CD, N, is_binary(CD)).

%% Intersperse a list of strings with Sep
-spec join([Str::unicode:chardata()], Sep::unicode:chardata()) -> [unicode:chardata()].
join([_]=String,_) ->
    String;
join([String|Strings], Sep) ->
    [String,Sep|join(Strings,Sep)];
join([], _) -> [].

%% Pad a string to desired length
-spec pad(unicode:chardata(), Length::integer()) -> unicode:charlist().
pad(CD, Length) ->
    pad(CD, Length, trailing, $\s).
-spec pad(unicode:chardata(), Length::integer(), Dir::direction()|'center') -> unicode:charlist().
pad(CD, Length, Dir) ->
    pad(CD, Length, Dir, $\s).
-spec pad(Str, Length, Dir, Char) -> unicode:charlist() when
      Str ::unicode:chardata(),
      Length :: integer(),
      Dir :: direction()|'center',
      Char :: grapheme_cluster().
pad(CD, Length, leading, Char) when is_integer(Length) ->
    Len = length(CD),
    [lists:duplicate(max(0, Length-Len), Char), CD];
pad(CD, Length, trailing, Char) when is_integer(Length) ->
    Len = length(CD),
    [CD|lists:duplicate(max(0, Length-Len), Char)];
pad(CD, Length, center, Char) when is_integer(Length) ->
    Len = length(CD),
    Size = max(0, Length-Len),
    Pre = lists:duplicate(Size div 2, Char),
    Post = case Size rem 2 of
               1 -> [Char];
               _ -> []
           end,
    [Pre, CD, Pre|Post].

%%  Strip characters from whitespace or Separator in Direction
-spec strip(Str::unicode:chardata()) -> unicode:chardata().
strip(Str) ->
    strip(Str, both, unicode_util:whitespace()).

-spec strip(Str::unicode:chardata(),direction()|'both') -> unicode:chardata().
strip(Str, Dir) ->
    strip(Str, Dir, unicode_util:whitespace()).

-spec strip(Str::unicode:chardata(),direction()|'both',[grapheme_cluster()]) ->
                   unicode:chardata().
strip(Str, leading, Sep) when is_list(Sep) ->
    strip_l(Str, Sep);
strip(Str, trailing, Sep) when is_list(Sep) ->
    strip_t(Str, 0, Sep);
strip(Str, both, Sep) when is_list(Sep) ->
    strip_t(strip_l(Str,Sep), 0, Sep).

%% Delete trailing newlines or \r\n
-spec chomp(Str::unicode:chardata()) -> unicode:chardata().
chomp(Str) ->
    strip_t(Str,0, [[$\r,$\n],$\n]).

%% Uppercase all chars in Str
-spec uppercase(Str::unicode:chardata()) -> unicode:chardata().
uppercase(CD) when is_list(CD) ->
    uppercase_list(CD);
uppercase(CD) when is_binary(CD) ->
    uppercase_bin(CD,<<>>).

%% Lowercase all chars in Str
-spec lowercase(Str::unicode:chardata()) -> unicode:chardata().
lowercase(CD) when is_list(CD) ->
    lowercase_list(CD);
lowercase(CD) when is_binary(CD) ->
    lowercase_bin(CD,<<>>).

%% Make a titlecase of the first char in Str
-spec titlecase(Str::unicode:chardata()) -> unicode:chardata().
titlecase(CD) when is_list(CD) ->
    case unicode_util:titlecase(CD) of
        [GC|Tail] -> concat(GC,Tail);
        Empty -> Empty
    end;
titlecase(CD) when is_binary(CD) ->
    case unicode_util:titlecase(CD) of
        [CP|Chars] when is_integer(CP) -> <<CP/utf8,Chars/binary>>;
        [CPs|Chars] ->
            << << <<CP/utf8>> || CP <- CPs>>/binary, Chars/binary>>;
        [] -> <<>>
    end.

%% Make a comparable string of the Str should be used for equality tests only
-spec casefold(Str::unicode:chardata()) -> unicode:chardata().
casefold(CD) when is_list(CD) ->
    casefold_list(CD);
casefold(CD) when is_binary(CD) ->
    casefold_bin(CD,<<>>).


%% Return the remaining string with prefix removed or else nomatch
-spec prefix(Str::unicode:chardata(), Prefix::unicode:chardata()) -> nomatch | unicode:chardata().
prefix(Str, Prefix0) ->
    Prefix = search_pattern(Prefix0),
    case prefix_1(Str, Prefix) of
        [] when is_binary(Str) -> <<>>;
        Res -> Res
    end.

%% split Haystack with the first occurrence of Needle, return list of splits
-spec split(Haystack::unicode:chardata(),
            Needle::unicode:chardata()) ->
                   [unicode:chardata()].
split(Haystack, Needle) ->
    split(Haystack, Needle, leading).

%% split Haystack with Needle, return list of splits
-spec split(Haystack::unicode:chardata(),
            Needle::unicode:chardata(),
            Where::direction()|'all') ->
                   [unicode:chardata()].
split(Haystack, Needle, Where) ->
    case is_empty(Needle) of
        true -> [Haystack];
        false ->
            case split_1(Haystack, search_pattern(Needle), 0, Where, [], []) of
                {_Curr, []} -> [Haystack];
                {_Curr, Acc} when Where =:= trailing -> Acc;
                {Curr, Acc} when Where =:= all -> lists:reverse([Curr|Acc]);
                Acc when is_list(Acc) -> Acc
            end
    end.

%% Replace the first Needle in Haystack with Replacement
-spec replace(Haystack::unicode:chardata(),
              Needle::unicode:chardata(),
              Replacement::unicode:chardata()) -> [unicode:chardata()].
replace(Haystack, Needle, Replacement) ->
    join(split(Haystack, Needle), Replacement).

%% Replace Where Needle in Haystack with Replacement
-spec replace(Haystack::unicode:chardata(),
              Needle::unicode:chardata(),
              Replacement::unicode:chardata(),
              Where::direction()|'all') -> [unicode:chardata()].
replace(Haystack, Needle, Replacement, Where) ->
    join(split(Haystack, Needle, Where), Replacement).

%% Split Str into a list of chardata separated by one of the grapheme clusters in Seps
-spec tokens(Str::unicode:chardata(), Seps::[grapheme_cluster()]) -> [unicode:chardata()].
tokens([], _) -> [];
tokens(Str, []) -> [Str];
%% tokens(Str, [C]) ->  % optimize
%%     lists:reverse(tokens_single_1(Str, C, [], []));
tokens(Str, Seps) when is_list(Seps) ->
    tokens_m(Str, Seps, []).

%% find first Needle in Haystack return rest of string
-spec find(Haystack::unicode:chardata(), Needle::unicode:chardata()) -> unicode:chardata().
find(Haystack, Needle) ->
    find(Haystack, Needle, leading).

%% find Needle in Haystack (search in Dir direction) return rest of string
-spec find(Haystack::unicode:chardata(), Needle::unicode:chardata(), Dir::direction()) -> unicode:chardata().
find(Haystack, "", _) -> Haystack;
find(Haystack, <<>>, _) -> Haystack;
find(Haystack, Needle, leading) ->
    find_l(Haystack, search_pattern(Needle));
find(Haystack, Needle, trailing) ->
    find_r(Haystack, search_pattern(Needle), Haystack).

%% Fetch first codepoint and return rest in tail
-spec gc(Str::unicode:chardata()) -> maybe_improper_list(grapheme_cluster(),unicode:chardata()).
gc(CD) -> unicode_util:gc(CD).

%% Fetch first grapheme cluster and return rest in tail
-spec cp(Str::unicode:chardata()) -> maybe_improper_list(char(),unicode:chardata()).
cp(CD) -> unicode_util:cp(CD).

%% Internals

length_1([_|Rest], N) ->
    length_1(unicode_util:gc(Rest), N+1);
length_1([], N) ->
    N.

equal_1(A, A) ->
    [];
equal_1([A|AR], [A|BR]) ->
    equal_1(AR, BR);
equal_1([A|AR], <<A/utf8, BR/binary>>) ->
    equal_1(AR, BR);
equal_1(<<A/utf8, BR/binary>>, [A|AR]) ->
    equal_1(AR, BR);
equal_1([A|AR], B) when not is_integer(A) ->
    case equal_1(A, B) of
        false -> false;
        BR -> equal_1(AR, BR)
    end;
equal_1(A, [B|_]=BR) when not is_integer(B) ->
    equal_1(BR,A);
equal_1([], BR) -> BR;
equal_1(_, _) -> false.

equal_nocase(A, A) -> true;
equal_nocase(A0, B0) ->
    case {cp(unicode_util:casefold(A0)), cp(unicode_util:casefold(B0))} of
        {[CP|A],[CP|B]} -> equal_nocase(A,B);
        {[], []} -> true;
        _ -> false
    end.

equal_norm(A, A, _Norm) -> true;
equal_norm(A0, B0, Norm) ->
    case {cp(unicode_util:Norm(A0)), cp(unicode_util:Norm(B0))} of
        {[CP|A],[CP|B]} -> equal_norm(A,B, Norm);
        {[], []} -> true;
        _ -> false
    end.

equal_norm_nocase(A, A, _Norm) -> true;
equal_norm_nocase(A0, B0, Norm) ->
    case {cp(unicode_util:casefold(unicode_util:Norm(A0))),
          cp(unicode_util:casefold(unicode_util:Norm(B0)))} of
        {[CP|A],[CP|B]} -> equal_norm_nocase(A,B, Norm);
        {[], []} -> true;
        _ -> false
    end.

reverse_1(CD, Acc) ->
    case unicode_util:gc(CD) of
        [GC|Rest] -> reverse_1(Rest, [GC|Acc]);
        [] -> Acc
    end.

slice_l(CD, N, Binary) when N > 0 ->
    case unicode_util:gc(CD) of
        [_|Cont] -> slice_l(Cont, N-1, Binary);
        [] when Binary -> <<>>;
        [] -> []
    end;
slice_l(Cont, 0, Binary) ->
    case is_empty(Cont) of
        true when Binary -> <<>>;
        _ -> Cont
    end.

slice_trail(CD, N) when is_list(CD) ->
    slice_list(CD, N);
slice_trail(CD, N) when is_binary(CD) ->
    slice_bin(CD, N, CD).

slice_list(CD, N) when N > 0 ->
    case unicode_util:gc(CD) of
        [GC|Cont] -> concat(GC, slice_list(Cont, N-1));
        [] -> []
    end;
slice_list(_, 0) ->
    [].

slice_bin(CD, N, Orig) when N > 0 ->
    case unicode_util:gc(CD) of
        [_|Cont] ->  slice_bin(Cont, N-1, Orig);
        [] -> Orig
    end;
slice_bin(CD, 0, Orig) ->
    Sz = byte_size(Orig) - byte_size(CD),
    <<Keep:Sz/binary, _/binary>> = Orig,
    Keep.

uppercase_list(CPs0) ->
    case unicode_util:uppercase(CPs0) of
        [Char|CPs] -> concat(Char,uppercase_list(CPs));
        [] -> []
    end.

uppercase_bin(CPs0, Acc) ->
    case unicode_util:uppercase(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            uppercase_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            uppercase_bin(CPs, <<Acc/binary, << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.

lowercase_list(CPs0) ->
    case unicode_util:lowercase(CPs0) of
        [Char|CPs] -> concat(Char,lowercase_list(CPs));
        [] -> []
    end.

lowercase_bin(CPs0, Acc) ->
    case unicode_util:lowercase(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            lowercase_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            lowercase_bin(CPs, <<Acc/binary, << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.

casefold_list(CPs0) ->
    case unicode_util:casefold(CPs0) of
        [Char|CPs] -> concat(Char, casefold_list(CPs));
        [] -> []
    end.

casefold_bin(CPs0, Acc) ->
    case unicode_util:casefold(CPs0) of
        [Char|CPs] when is_integer(Char) ->
            casefold_bin(CPs, <<Acc/binary, Char/utf8>>);
        [Chars|CPs] ->
            casefold_bin(CPs, <<Acc/binary, << <<CP/utf8>> || CP <- Chars>>/binary >>);
        [] -> Acc
    end.


strip_l([Bin|Cont0], Sep) when is_binary(Bin) ->
    case bin_search(Bin, Cont0, Sep, true) of
        {nomatch, Cont} -> strip_l(Cont, Sep);
        Keep -> Keep
    end;
strip_l(Str, Sep) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, Sep) of
                true -> strip_l(Cs, Sep);
                false -> Str
            end;
        [] -> []
    end;
strip_l(Bin, Sep) when is_binary(Bin) ->
    case bin_search(Bin, Sep, true) of
        {nomatch,_} -> <<>>;
        [Keep] -> Keep
    end.

strip_t([Bin|Cont0], N, Sep) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search(Rest, Cont0, Sep, false) of
        {nomatch,_} ->
            stack(Bin, strip_t(Cont0, 0, Sep));
        [SepStart|Cont1] ->
            case bin_search(SepStart, Cont1, Sep, true) of
                {nomatch, Cont} ->
                    Tail = strip_t(Cont, 0, Sep),
                    case is_empty(Tail) of
                        true ->
                            KeepSz = byte_size(Bin) - byte_size(SepStart),
                            <<Keep:KeepSz/binary, _/binary>> = Bin,
                            Keep;
                        false ->
                            stack(Bin, Tail)
                    end;
                [NonSep|Cont] when is_binary(NonSep) ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    strip_t([Bin|Cont], KeepSz, Sep)
            end
    end;
strip_t(Str, 0, Sep) when is_list(Str) ->
    case unicode_util:gc(Str) of
        [C|Cs] ->
            case lists:member(C, Sep) of
                true ->
                    Tail = strip_t(Cs, 0, Sep),
                    case is_empty(Tail) of
                        true -> [];
                        false -> concat(C,Tail)
                    end;
                false ->
                    concat(C,strip_t(Cs, 0, Sep))
            end;
        [] -> []
    end;
strip_t(Bin, N, Sep) when is_binary(Bin) ->
    <<_:N/binary, Rest/binary>> = Bin,
    case bin_search(Rest, Sep, false) of
        {nomatch,_} -> Bin;
        [SepStart] ->
            case bin_search(SepStart, Sep, true) of
                {nomatch,_} ->
                    KeepSz = byte_size(Bin) - byte_size(SepStart),
                    <<Keep:KeepSz/binary, _/binary>> = Bin,
                    Keep;
                [NonSep] ->
                    KeepSz = byte_size(Bin) - byte_size(NonSep),
                    strip_t(Bin, KeepSz, Sep)
            end
    end.

search_pattern([]) ->
    [];
search_pattern(CD) ->
    unicode:characters_to_list(CD).

prefix_1(Cs, []) -> Cs;
prefix_1(Cs, [_]=Pre) ->
    prefix_2(unicode_util:gc(Cs), Pre);
prefix_1(Cs, Pre) ->
    prefix_2(unicode_util:cp(Cs), Pre).

prefix_2([C|Cs], [C|Pre]) ->
    prefix_1(Cs, Pre);
prefix_2(_, _) ->
    nomatch.

split_1([Bin|Cont0], Needle, Start, Where, Curr0, Acc)
  when is_binary(Bin) ->
    case bin_search_str(Bin, Start, Cont0, Needle) of
        {nomatch,Sz,Cont} ->
            <<Keep:Sz/binary, _/binary>> = Bin,
            split_1(Cont, Needle, 0, Where, [Keep|Curr0], Acc);
        {Before, [Cs0|Cont], After} ->
            Curr = [Before|Curr0],
            case Where of
                leading ->
                    [rev(Curr),After];
                trailing ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    Next = byte_size(Bin) - byte_size(Cs),
                    split_1([Bin|Cont], Needle, Next, Where,
                            Curr0, [rev(Curr),After]);
                all ->
                    %Next = byte_size(Bin) - byte_size(After),
                    split_1(After, Needle, 0, Where, [], [rev(Curr)|Acc])
            end
    end;
split_1(Cs0, [C|_]=Needle, _, Where, Curr, Acc) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs0, Needle) of
                nomatch -> split_1(Cs, Needle, 0, Where, concat(C,Curr), Acc);
                Rest when Where =:= leading ->
                    [rev(Curr), Rest];
                Rest when Where =:= trailing ->
                    split_1(Cs, Needle, 0, Where, [C|Curr], [rev(Curr), Rest]);
                Rest when Where =:= all ->
                    split_1(Rest, Needle, 0, Where, [], [rev(Curr)|Acc])
            end;
        [Other|Cs] ->
            split_1(Cs, Needle, 0, Where, concat(Other,Curr), Acc);
        [] ->
            {rev(Curr), Acc}
    end;
split_1(Bin, [_C|_]=Needle, Start, Where, Curr0, Acc) ->
    case bin_search_str(Bin, Start, [], Needle) of
        {nomatch,_,_} ->
            <<_:Start/binary, Keep/binary>> = Bin,
            {rev([Keep|Curr0]), Acc};
        {Before, [Cs0], After} ->
            case Where of
                leading ->
                    [rev([Before|Curr0]),After];
                trailing ->
                    <<_/utf8, Cs/binary>> = Cs0,
                    Next = byte_size(Bin) - byte_size(Cs),
                    split_1(Bin, Needle, Next, Where, Curr0, [stack(Curr0,Before),After]);
                all ->
                    Next = byte_size(Bin) - byte_size(After),
                    <<_:Start/binary, Keep/binary>> = Before,
                    Curr = [Keep|Curr0],
                    split_1(Bin, Needle, Next, Where, [], [rev(Curr)|Acc])
            end
    end.

tokens_m([Bin|Cont0], Seps, Ts) when is_binary(Bin) ->
    case bin_search(Bin, Cont0, Seps, true) of
        {nomatch,Cont} ->
            tokens_m(Cont, Seps, Ts);
        Cs ->
            {Token,Rest} = token_pick(Cs, Seps, []),
            tokens_m(Rest, Seps, [Token|Ts])
    end;
tokens_m(Cs0, Seps, Ts) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, Seps) of
                true  ->
                    tokens_m(Cs, Seps, Ts);
                false ->
                    {Token,Rest} = token_pick(Cs0, Seps, []),
                    tokens_m(Rest, Seps, [Token|Ts])
            end;
        [] ->
            lists:reverse(Ts)
    end;
tokens_m(Bin, Seps, Ts) when is_binary(Bin) ->
    case bin_search(Bin, Seps, true) of
        {nomatch,_} ->
            lists:reverse(Ts);
        [Cs] ->
            {Token,Rest} = token_pick(Cs, Seps, []),
            tokens_m(Rest, Seps, [Token|Ts])
    end.

token_pick([Bin|Cont0], Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, Cont0, Seps, false) of
        {nomatch,_} ->
            token_pick(Cont0, Seps, [Bin|Tkn]);
        [Left|_Cont] = Cs ->
            Bytes = byte_size(Bin) - byte_size(Left),
            <<Token:Bytes/binary, _/binary>> = Bin,
            {btoken(Token, Tkn), Cs}
    end;
token_pick(Cs0, Seps, Tkn) when is_list(Cs0) ->
    case unicode_util:gc(Cs0) of
        [C|Cs] ->
            case lists:member(C, Seps) of
                true  -> {rev(Tkn), Cs0};
                false -> token_pick(Cs, Seps, concat(rev(C),Tkn))
            end;
        [] ->
            {rev(Tkn), []}
    end;
token_pick(Bin, Seps, Tkn) when is_binary(Bin) ->
    case bin_search(Bin, Seps, false) of
        {nomatch,_} ->
            {btoken(Bin,Tkn), []};
        [Left] ->
            Bytes = byte_size(Bin) - byte_size(Left),
            <<Token:Bytes/binary, _/binary>> = Bin,
            {btoken(Token, Tkn), Left}
    end.

find_l([Bin|Cont0], Needle) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch, _, Cont} ->
            find_l(Cont, Needle);
        {_Before, Cs, _After} ->
            Cs
    end;
find_l(Cs0, [C|_]=Needle) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs0, Needle) of
                nomatch -> find_l(Cs, Needle);
                _ -> Cs0
            end;
        [_C|Cs] ->
            find_l(Cs, Needle);
        [] -> []
    end;
find_l(Bin, Needle) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch,_,_} -> <<>>;
        {_Before, [Cs], _After} -> Cs
    end.

find_r([Bin|Cont0], Needle, Res) when is_binary(Bin) ->
    case bin_search_str(Bin, 0, Cont0, Needle) of
        {nomatch,_,Cont} ->
            find_r(Cont, Needle, Res);
        {_, Cs0, _} ->
            [_|Cs] = unicode_util:gc(Cs0),
            find_r(Cs, Needle, Cs0)
    end;
find_r(Cs0, [C|Pre]=Needle, Res) when is_list(Cs0) ->
    case unicode_util:cp(Cs0) of
        [C|Cs] ->
            case prefix_1(Cs, Pre) of
                nomatch -> find_r(Cs, Needle, Res);
                _ -> find_r(Cs, Needle, Cs0)
            end;
        [_C|Cs] ->
            find_r(Cs, Needle, Res);
        [] -> Res
    end;
find_r(Bin, Needle, Res) ->
    case bin_search_str(Bin, 0, [], Needle) of
        {nomatch,_,_} -> Res;
        {_Before, [Cs0], _After} ->
            <<_/utf8, Cs/binary>> = Cs0,
            find_r(Cs, Needle, Cs0)
    end.

%% These are used to avoid creating lists around binaries
%% might be unnecessary, or show that there is a better solution?
btoken(<<>>, Tkn) -> rev(Tkn);
btoken(Token, []) -> Token;
btoken(BinPart, Tkn) -> rev(stack(BinPart,Tkn)).

rev([B]) when is_binary(B) -> B;
rev(L) when is_list(L) -> lists:reverse(L);
rev(C) when is_integer(C) -> C.

stack(Bin, []) -> Bin;
stack(<<>>, St) -> St;
stack([], St) -> St;
stack(Bin, St) -> [Bin|St].

%% Binary special
bin_search(Bin, Seps, Invert) ->
    bin_search(Bin, [], Seps, Invert).

bin_search(Bin, Cont, [], Invert) ->
    case Invert of
        true  -> [Bin|Cont];
        false -> {nomatch,Cont}
    end;
bin_search(Bin, Cont, Seps, false) ->
    bin_search_loop(Bin, bin_pattern(Seps), Cont, Seps);
bin_search(Bin, Cont, [Sep], Invert) ->
    bin_search_1([Bin|Cont], Sep, Invert);
bin_search(Bin, Cont, [_|_]=Seps, Invert) ->
    bin_search_n([Bin|Cont], Seps, Invert).

%% Need to work with [<<$a>>, <<778/utf8>>],
%% i.e. å in nfd form  $a "COMBINING RING ABOVE"
%% and PREPEND characters like "ARABIC NUMBER SIGN" 1536 <<216,128>>
%% combined with other characters are currently ignored.
bin_pattern([CP|Seps]) when is_integer(CP) ->
    [<<CP/utf8>>|bin_pattern(Seps)];
bin_pattern([[CP|_]|Seps]) ->
    [<<CP/utf8>>|bin_pattern(Seps)];
bin_pattern([]) -> [].

bin_search_loop(Bin, BinSeps, Cont, Seps) ->
    case binary:match(Bin, BinSeps) of
        nomatch -> {nomatch,Cont};
        {Start, _Bytes} ->
            <<_:Start/binary, Cont0/binary>> = Bin,
            Cont1 = stack(Cont0, Cont),
            [GC|Cont2] = unicode_util:gc(Cont1),
            case lists:member(GC, Seps) of
                false when is_binary(Cont2) ->
                    bin_search_loop(Cont2, BinSeps, Cont, Seps);
                true when is_list(Cont1) ->
                    Cont1;
                true ->
                    [Cont1];
                _ ->
                    {nomatch, Cont1}
            end
    end.

bin_search_1([<<>>|CPs], _, _) ->
    {nomatch, CPs};
bin_search_1([_|_]=CPs, Sep, Inv) ->
    [C|Cs] = unicode_util:gc(CPs),
    case Inv xor (C =:= Sep) of
        %% true when is_binary(CPs) -> [CPs];
        true -> CPs;
        false when is_binary(hd(Cs)) ->
            bin_search_1(Cs, Sep, Inv);
        false ->
            {nomatch, Cs}
    end.

bin_search_n([<<>>|CPs], _, _) ->
    {nomatch, CPs};
bin_search_n([_|_]=CPs, Seps, Inv) ->
    [C|Cs] = unicode_util:gc(CPs),
    case Inv xor lists:member(C, Seps) of
        %% true when is_binary(Bin) -> [Bin];
        true -> CPs;
        false when is_binary(hd(Cs)) ->
            bin_search_n(Cs, Seps, Inv);
        false -> {nomatch, Cs}
    end.

bin_search_str(Bin0, Start, Cont, [CP|_]=SearchCPs) ->
    <<_:Start/binary, Bin/binary>> = Bin0,
    case binary:match(Bin, <<CP/utf8>>) of
        nomatch -> {nomatch, byte_size(Bin0), Cont};
        {Where0, _} ->
            Where = Start+Where0,
            <<Keep:Where/binary, Cs0/binary>> = Bin0,
            [GC|Cs]=unicode_util:gc(Cs0),
            case prefix_1(stack(Cs0,Cont), SearchCPs) of
                nomatch when is_binary(Cs) ->
                    KeepSz = byte_size(Bin0) - byte_size(Cs),
                    bin_search_str(Bin0, KeepSz, Cont, SearchCPs);
                nomatch ->
                    {nomatch, Where, stack([GC|Cs],Cont)};
                [] ->
                    {Keep, [Cs0|Cont], <<>>};
                Rest ->
                    {Keep, [Cs0|Cont], Rest}
            end
    end.
