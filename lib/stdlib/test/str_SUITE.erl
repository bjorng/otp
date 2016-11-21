%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%% Purpose: string test suite.
%%%-----------------------------------------------------------------
-module(str_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([debug/0]).

%% Test cases must be exported.
-export([is_empty/1, length/1, to_graphemes/1,
         concat/1, reverse/1, slice/1, join/1,
         equal/1,
         pad/1, strip/1, chomp/1,
         uppercase/1, lowercase/1, titlecase/1, casefold/1,
         prefix/1, split/1, replace/1, find/1,
         tokens/1, cd_gc/1, meas/1
        ]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [is_empty, length, to_graphemes,
     equal, concat, reverse, slice, join,
     pad, strip, chomp,
     tokens,
     uppercase, lowercase, titlecase, casefold,
     prefix, find, split, replace, cd_gc,
     meas
    ].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

-define(TEST(B,C,D), test(?LINE,?FUNCTION_NAME,B,C,D, true)).
-define(TEST_NN(B,C,D), test(?LINE,?FUNCTION_NAME,B,C,D, false)).

debug() ->
    Config = [{data_dir, ?MODULE_STRING++"_data"}],
    [io:format("~p:~p~n",[Test,?MODULE:Test(Config)]) || Test <- all()].

is_empty(_) ->
    ?TEST("", [], true),
    ?TEST([""|<<>>], [], true),
    ?TEST("a", [], false),
    ?TEST([""|<<$a>>], [], false),
    ?TEST(["",[<<>>]], [], true),
    ok.

length(_) ->
    %% invalid arg type
    {'EXIT',_} = (catch str:len({})),
    {'EXIT',_} = (catch str:len(foo)),
    %% Valid signs
    ?TEST("", [], 0),
    ?TEST([""|<<>>], [], 0),
    L = tuple_size(list_to_tuple(atom_to_list(?MODULE))),
    ?TEST(atom_to_list(?MODULE), [], L),
    ?TEST("Hello", [], 5),
    ?TEST("UC Ω ßð", [], 7),
    ?TEST(["abc"|<<"abc">>], [], 6),
    ?TEST(["abc",["def"]], [], 6),
    ?TEST([<<97/utf8, 778/utf8, 98/utf8>>, [776,111,776]], [], 3), %% åäö in nfd
    ok.

equal(_) ->
    %% invalid arg type
    false = (catch str:equal(1, 2)),
    {'EXIT',_} = (catch str:equal(1, 2, foo)),
    {'EXIT',_} = (catch str:equal(1, 2, true, foo)),

    true = str:equal(2, 2),	% not good, should crash
    ?TEST("", [<<"">>], true),
    ?TEST("Hello", ["Hello"], true),
    ?TEST("Hello", ["Hell"], false),
    ?TEST("Hello", ["Hello!"], false),
    ?TEST("Hello", [<<"Hello"/utf8>>], true),
    ?TEST("Hello", [<<"Mello"/utf8>>], false),
    ?TEST("Hello", [<<"Hello!"/utf8>>], false),
    ?TEST(["Hello",[" deep"]], ["Hello deep"], true),
    ?TEST(["Hello",[<<" deep"/utf8>>]], ["Hello deep"], true),
    ?TEST("Hello deep", [["Hello", [" deep"]]], true),
    ?TEST("Hello deep", [["Hello", [" d!eep"]]], false),
    ?TEST("Hello deep", [["Hello", [<<" deep"/utf8>>]]], true),
    false = str:equal("Åäö", [<<97/utf8, 778/utf8, 98/utf8>>, [776,111,776]]), %% nfc vs nfd

    %% case_insensitive_equal()
    ?TEST("", ["", true], true),
    ?TEST("a", ["b", true], false),
    ?TEST("", [<<>>, true], true),
    ?TEST("", [[<<>>,[]], true], true),
    ?TEST("", [[<<>>,[$a]], true], false),
    ?TEST("123", ["123", true], true),
    ?TEST("abc", ["abc", true], true),
    ?TEST([[],<<>>,"ABC"|<<>>], [["abc",[]], true], true),
    ?TEST("ABCa", ["abcå", true], false),
    ?TEST("åäö", [{norm,"åäö"}, true], true),
    ?TEST("ÅÄÖ", [{norm,"åäö"}, true], true),
    ?TEST("MICHAŁ", ["michał", true], true),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], ["michał", true], true),
    ?TEST("ß SHARP S", ["ss sharp s", true], true),
    ?TEST("ẞ SHARP S", [[<<$ß/utf8, $\s>>,"SHARP S"], true], true),
    ?TEST("ẞ SHARP ß", ["ss sharp s", true], false),
    ?TEST(<<"İ I WITH DOT ABOVE"/utf8>>, ["i̇ i with dot above", true], true),
    %% These should be equivalent with the above
    true = str:equal(str:casefold(["Mic",<<"HAŁ"/utf8>>]), str:casefold("michał")),
    true = str:equal(str:casefold("ẞ SHARP S"), str:casefold([<<$ß/utf8, $\s>>,"SHARP S"])),
    false = str:equal(str:casefold("ẞ SHARP ß"), str:casefold("ss sharp s")),

    %% Normalization
    ?TEST_NN("", ["", true, none], true),
    ?TEST_NN("a", ["b", true, nfc], false),
    ?TEST_NN("a", ["b", true, nfd], false),
    ?TEST_NN("a", ["b", true, nfkc], false),
    ?TEST_NN("a", ["b", true, nfkd], false),

    ?TEST_NN("a", ["A", false, nfc], false),
    ?TEST_NN("a", ["A", false, nfd], false),
    ?TEST_NN([<<>>,"a"|<<>>], ["A", true, nfkc], true),
    ?TEST_NN(<<"a">>, ["A", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, none], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfd], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfkc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfkd], true),

    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, none], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", false, nfc], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfd], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, none], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfc], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfd], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, none], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfc], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfd], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfkd], true),

    ok.

to_graphemes(_) ->
    %% More tests are in unicode_util_SUITE.erl
    {'EXIT', _} = (catch unicode:characters_to_nfd_binary(["asdåäö", an_atom])),
    String = ["abc..åäö", $e, 788, <<"Ωµe`è"/utf8>>, "œŒþæÆħ§ß"],
    NFD = unicode:characters_to_nfd_list(String),
    [] = str:to_graphemes([]),
    [] = str:to_graphemes(<<>>),
    GCs = str:to_graphemes(String),
    true = erlang:length(GCs) =:= str:length(String),
    true = erlang:length(GCs) =:= erlang:length(str:to_graphemes(NFD)),
    true = erlang:length(GCs) =:=
        erlang:length(str:to_graphemes(unicode:characters_to_nfc_list(String))),
    ok.

concat(_) ->
    %% invalid arg type
    {'EXIT',_} = (catch str:concat(kalle, 2)),
    Str1 = "Hello ",
    Str2 = "Ω ßð",
    "hi" = str:concat("", "hi"),
    "hi" = str:concat("hi", ""),
    <<"hi">> = str:concat(<<>>, <<"hi">>),
    <<"hi">> = str:concat(<<"hi">>, <<>>),
    ?TEST(Str1, [Str2], "Hello Ω ßð"),
    ?TEST(Str1, [<<"Ω ßð"/utf8>>], "Hello Ω ßð"),
    ok.

reverse(_) ->
    {'EXIT',_} = (catch str:reverse(2)),
    Str1 = "Hello ",
    Str2 = "Ω ßð",
    Str3 = "åäö",
    ?TEST("", [], ""),
    ?TEST(Str1, [], lists:reverse(Str1)),
    ?TEST(Str2, [], lists:reverse(Str2)),
    ?TEST(Str3, [], lists:reverse(Str3)),
    true = str:reverse(Str3) =:= lists:reverse(str:to_graphemes(Str3)),
    ok.

slice(_) ->
    {'EXIT',_} = (catch str:slice(2, 2, 2)),
    {'EXIT',_} = (catch str:slice("asd", foo, 2)),
    {'EXIT',_} = (catch str:slice("asd", 2, -1)),
    ?TEST("", [3], ""),
    ?TEST("aåä", [3], ""),
    ?TEST("aåäöbcd", [3], "öbcd"),
    ?TEST([<<"aå"/utf8>>,"äöbcd"], [3], "öbcd"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3], "öbcd"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3, infinity], "öbcd"),

    ?TEST("", [3, 2], ""),
    ?TEST("aåä", [3, 2], ""),
    ?TEST("aåäöbcd", [3,2], "öb"),
    ?TEST([<<"aå"/utf8>>,"äöbcd"], [3,3], "öbc"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3,10], "öbcd"),

    ok.

join(_) ->
    Res = "Hi_there_how_are_you",
    true = res(str:join([], "_"), ""),
    true = res(str:join(["Hi"], "_"), "Hi"),
    true = res(str:join(["Hi", "there", "how", "are", "you"], "_"), Res),
    ok.

pad(_) ->
    Str = "Hallå",
    ?TEST(Str, [7], "Hallå  "),
    ?TEST(Str, [7, leading], "  Hallå"),
    ?TEST(Str, [4, center, $.], "Hallå"),
    ?TEST(Str, [10, center, $.], "..Hallå..."),
    ?TEST(Str, [10, leading, $.], ".....Hallå"),
    ?TEST(Str, [10, trailing, $.], "Hallå....."),
    ?TEST(Str++["f"], [10, trailing, $.], "Hallåf...."),
    ?TEST(Str++[" flåwer"], [10, trailing, $.], "Hallå flåwer"),
    ok.

strip(_) ->
    Str = "\t\s..Ha\s.llå..\t\n\r",
    ?TEST("", [], ""),
    ?TEST(Str, [both, "x"], Str),
    ?TEST(Str, [leading], "..Ha\s.llå..\t\n\r"),
    ?TEST(Str, [trailing], "\t\s..Ha\s.llå.."),
    ?TEST(Str, [], "..Ha .llå.."),
    ?TEST(".. ", [both, ""], ".. "),
    ?TEST([<<".. ">>], [both, ". "], ""),
    ?TEST(".. h.ej ..", [leading, ". "], "h.ej .."),
    ?TEST(".. h.ej ..", [trailing, ". "], ".. h.ej"),
    ?TEST(".. h.ej ..", [both, ". "], "h.ej"),
    ?TEST(["..", <<"h.ej">>, ".."], [both, ". "], "h.ej"),
    ?TEST([[], "..", " h.ej ", <<"..">>], [both, ". "], "h.ej"),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [both, ". "], "h.ej"),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [trailing, ". "], ".. h.ej"),
    ?TEST([<<"..  h.ej .">>, <<"..">>], [both, ". "], "h.ej"),
    ?TEST(["..h", ".e", <<"j..">>], [both, ". "], "h.ej"),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [both, ". "], "h.ejsan"),
    %% Test that it behaves with graphemes (i.e. nfd tests are the hard part)
    ?TEST("aaåaa", [both, "a"], "å"),
    ?TEST(["aaa",778,"äöoo"], [both, "ao"], "åäö"),
    ?TEST([<<"aaa">>,778,"äöoo"], [both, "ao"], "åäö"),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>], [both, [[$e,778]]], "åäö"),
    ok.

chomp(_) ->
    Str = "åäö\na\r\nsd\n",
    Res = "åäö\na\r\nsd",
    ?TEST("", [], ""),
    ?TEST("\n", [], ""),
    ?TEST("str \t", [], "str \t"),
    ?TEST("str \t\n\r", [], "str \t\n\r"),
    ?TEST(Str, [], Res),
    ?TEST([Str,$\n], [], Res),
    ?TEST([Str|"\n"], [], Res),
    ?TEST([Str|<<"\n">>], [], Res),
    ?TEST([Str,$\r|<<"\n">>], [], Res),
    ?TEST([Str, <<$\r>>|"\n"], [], Res),
    ok.

uppercase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "ABC"),
    ?TEST("ABC", [], "ABC"),
    ?TEST("abcdefghiljklmnopqrstvxyzåäö",[], "ABCDEFGHILJKLMNOPQRSTVXYZÅÄÖ"),
    ?TEST("åäö", [], "ÅÄÖ"),
    ?TEST("ÅÄÖ", [], "ÅÄÖ"),
    ?TEST("Michał", [], "MICHAŁ"),
    ?TEST(["Mic",<<"hał"/utf8>>], [], "MICHAŁ"),
    ?TEST("ǉǇ", [], "ǇǇ"),
    ?TEST("Ǉǉ", [], "ǇǇ"),
    ?TEST("ß sharp s", [], "SS SHARP S"),
    ok.

lowercase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "abc"),
    ?TEST("ABC", [], "abc"),
    ?TEST("åäö", [], "åäö"),
    ?TEST("ÅÄÖ", [], "åäö"),
    ?TEST("MICHAŁ", [], "michał"),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], [], "michał"),
    ?TEST("ß SHARP S", [], "ß sharp s"),
    ?TEST("İ I WITH DOT ABOVE", [], "i̇ i with dot above"),
    ok.

titlecase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    %% Titlecase is the same as uppercase for most chars
    [?TEST([C,$x], [], [str:uppercase([C]),$x]) ||
        C <-"abcdefghiljklmnopqrstvxyzåäö"],
    %% Example of a different mapping
    ?TEST("ǉusad", [],"ǈusad"),
    ?TEST("ǉǇ", [], "ǈǇ"),
    ?TEST("Ǉǉ", [], "ǈǉ"),
    ?TEST("ß sharp s", [], "Ss sharp s"),
    ok.

casefold(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "abc"),
    ?TEST("ABC", [], "abc"),
    ?TEST("åäö", [], "åäö"),
    ?TEST("ÅÄÖ", [], "åäö"),
    ?TEST("MICHAŁ", [], "michał"),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], [], "michał"),
    ?TEST("ß SHARP S", [], "ss sharp s"),
    ?TEST("ẞ SHARP S", [], "ss sharp s"),
    ?TEST("İ I WITH DOT ABOVE", [], "i̇ i with dot above"),
    ok.

prefix(_) ->
    ?TEST("", ["a"], nomatch),
    ?TEST("a", [""], "a"),
    ?TEST("b", ["a"], nomatch),
    ?TEST("a", ["a"], ""),
    ?TEST("å", ["a"], nomatch),
    ?TEST(["a",<<778/utf8>>], ["a"], nomatch),
    ?TEST([<<"a"/utf8>>,778], ["a"], nomatch),
    ?TEST("hejsan", [""], "hejsan"),
    ?TEST("hejsan", ["hej"], "san"),
    ?TEST("hejsan", ["hes"], nomatch),
    ?TEST(["h", "ejsan"], ["hej"], "san"),
    ?TEST(["h", "e", "jsan"], ["hej"], "san"),
    ?TEST(["h", "e", "san"], ["hej"], nomatch),
    ?TEST(["h", <<"ejsan">>], ["hej"], "san"),
    ?TEST(["h", <<"e">>, "jsan"], ["hej"], "san"),
    ?TEST(["h", "e", <<"jsan">>], ["hej"], "san"),
    ok.

split(_) ->
    Mod = fun(Res) ->
                  [lists:flatten(unicode:characters_to_nfc_list(io_lib:format("~ts", [Str])))
                   || Str <- Res] end,
    ?TEST("..", ["", leading], {Mod, [".."]}),
    ?TEST("..", ["..", leading], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", leading], {Mod, ["abcd"]}),
    ?TEST("ab..bc", ["..", leading], {Mod, ["ab","bc"]}),
    ?TEST("ab..bc..cd", ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST("..ab", [".."], {Mod, [[],"ab"]}),
    ?TEST("ab..", ["..", leading], {Mod, ["ab",[]]}),
    ?TEST(["ab..bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab","..bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab",<<"..bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.",".bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.",<<".bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab..","bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab..",<<"bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.","bc..cd"], ["..", leading], {Mod, ["ab.bc","cd"]}),
    ?TEST("ab...bc", ["..", leading], {Mod, ["ab",".bc"]}),

    ?TEST("..", ["", trailing], {Mod, [".."]}),
    ?TEST("..", ["..", trailing], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", trailing], {Mod, ["abcd"]}),
    ?TEST("ab..bc", ["..", trailing], {Mod, ["ab","bc"]}),
    ?TEST("ab..bc..cd", ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST("..ab", ["..", trailing], {Mod, [[],"ab"]}),
    ?TEST("ab..", ["..", trailing], {Mod, ["ab",[]]}),
    ?TEST(["ab..bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab","..bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab",<<"..bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST([<<"ab.">>,".bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab.",<<".bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab..","bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab..",<<"bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab.","bc..cd"], ["..", trailing], {Mod, ["ab.bc","cd"]}),
    ?TEST("ab...bc", ["..", trailing], {Mod, ["ab.","bc"]}),

    ?TEST("..", ["..", all], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", all], {Mod, ["abcd"]}),
    ?TEST("a..b", ["..", all], {Mod, ["a","b"]}),
    ?TEST("a..b..c", ["..", all], {Mod, ["a","b","c"]}),
    ?TEST("a..", ["..", all], {Mod, ["a",[]]}),
    ?TEST(["a..b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a","..b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a",<<"..b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.",".b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.",<<".b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a..","b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a..",<<"b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.","b..c"], ["..", all], {Mod, ["a.b","c"]}),
    ?TEST("a...b", ["..", all], {Mod, ["a",".b"]}),

    %% Grapheme (split) tests
    ?TEST("aΩΩb", ["Ω", all], {Mod, ["a","","b"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], leading], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], trailing], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], all], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", leading], {Mod, [[$a, $a, $e,778,$ö],"eåäö"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", trailing], {Mod, [[$a, $a, $e,778,$ö, $e],"åäö"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", all], {Mod, [[$a, $a, $e,778,$ö],"", "åäö"]}),

    ok.

replace(_) ->
    ?TEST(["a..b.", [".c"]], ["xxx", "::"], "a..b..c"),
    ?TEST(["a..b.", [".c"]], ["..", "::"], "a::b..c"),
    ?TEST([<<"a..b.">>, [".c"]], ["..", "::", trailing], "a..b::c"),
    ?TEST(["a..b.", [".c"]], ["..", "::", all], "a::b::c"),
    ok.

cd_gc(_) ->
    [] = str:cp(""),
    [] = str:cp(<<>>),
    [] = str:cp([<<>>]),
    "abcd" = str:cp("abcd"),
    [$e,778] = str:cp([$e,778]),
    [$e|<<204,138>>] = str:cp(<<$e,778/utf8>>),
    [778|_] = str:cp(tl(str:cp(<<$e,778/utf8>>))),

    [] = str:gc(""),
    [] = str:gc(<<>>),
    [] = str:gc([<<>>]),
    "abcd" = str:gc("abcd"),
    [[$e,778]] = str:gc([$e,778]),
    [[$e,778]] = str:gc(<<$e,778/utf8>>),

    ok.


find(_) ->
    ?TEST(["h", "ejsan"], [""], "hejsan"),
    ?TEST(["h", "ejsan"], [<<>>], "hejsan"),
    ?TEST([], [""], ""),
    ?TEST([], ["hej"], ""),
    ?TEST(["h", "ejsan"], ["hej"], "hejsan"),
    ?TEST(["h", "e", "jsan"], ["hej"], "hejsan"),
    ?TEST(["xh", "e", "san"], ["hej"], ""),
    ?TEST([<<"xh">>, <<"ejsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", <<"ejsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", <<"e">>, "jsan"], ["hej"], "hejsan"),
    ?TEST(["xh", "e", <<"jsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", "er", <<"ljsane">>, "rlang"], ["erl", leading], "erljsanerlang"),
    ?TEST("aΩΩb", ["Ω", leading], "ΩΩb"),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], leading], [$e,778]++"äöoo"),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", leading], "eeåäö"),

    ?TEST(["h", "ejsan"], ["", trailing], "hejsan"),
    ?TEST([], ["", trailing], ""),
    ?TEST([], ["hej", trailing], ""),
    ?TEST(["h", "ejsan"], ["hej", trailing], "hejsan"),
    ?TEST(["h", "e", "jsan"], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "e", "san"], ["hej", trailing], "xhesan"),
    ?TEST([<<"xh">>, <<"ejsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", <<"ejsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", <<"e">>, "jsan"], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "e", <<"jsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "er", <<"ljsane">>, "rlang"], ["erl", trailing], "erlang"),
    ?TEST("aΩΩb", ["Ω", trailing], "Ωb"),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], trailing], [$e,778]++"äöoo"),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", trailing], "eåäö"),

    ok.

tokens(_) ->
    Mod = fun(Res) ->
                  [lists:flatten(unicode:characters_to_nfc_list(io_lib:format("~ts", [Str])))
                   || Str <- Res]
          end,
    Res = ["Hej", "san", "Hopp", "san"],
    ?TEST( "", [" ,."],  {Mod, []}),
    ?TEST( "Hej san", [""],  {Mod, ["Hej san"]}),
    ?TEST( "  ,., ", [" ,."],  {Mod, []}),
    ?TEST( "Hej san Hopp san", [" ,."], {Mod, Res}),
    ?TEST(" Hej san Hopp san ", [" ,."], {Mod, Res}),
    ?TEST(" Hej san, .Hopp san ", [" ,."], {Mod, Res}),

    ?TEST([" Hej san",", .Hopp san "], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa","n, .Hopp san "], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,"," .Hopp san "], [" ,."], {Mod, Res}),

    ?TEST([" Hej san",[", .Hopp san "]], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",["n, .Hopp san "]], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",[" .Hopp san "]], [" ,."], {Mod, Res}),

    ?TEST([" Hej san",<<", .Hopp "/utf8>>, "san"], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",<<"n, .Hopp"/utf8>>, " san"], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",<<" .Hopp s"/utf8>>, "an"], [" ,."], {Mod, Res}),
    ?TEST([" Hej san",[<<", .Hopp san "/utf8>>]], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",[<<"n, .Hopp san "/utf8>>]], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",[<<" .Hopp san "/utf8>>], <<"  ">>], [" ,."], {Mod, Res}),

    ?TEST(["b1ec1e",778,"äöo21"], ["eo"], {Mod, ["b1",[$c,$1,$e,778,$ä,$ö],"21"]}),
    ?TEST([<<"b1ec1e">>,778,"äöo21"], ["eo"], {Mod, ["b1",[$c,$1,$e,778,$ä,$ö],"21"]}),
    %% Grapheme (split) tests
    ?TEST("a1Ωb1Ωc1", ["Ω"], {Mod, ["a1","b1","c1"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[[$e,778]]], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöo21"], [[[$e,778],$o]], {Mod, ["aa","äö","21"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e"], {Mod, [[$a, $a, $e,778,$ö],"åäö"]}),
    ok.


meas(Config) ->
    TestDir = filename:dirname(str:strip(proplists:get_value(data_dir, Config), trailing, "/")),
    File =  filename:join(TestDir, ?MODULE_STRING ++ ".erl"),
    io:format("File ~s ",[File]),
    {ok, Bin} = file:read_file(File),
    io:format("~p~n",[byte_size(Bin)]),
    Do = fun(Name, Func, Mode) ->
                 {N, Mean, Stddev, _} = time_func(Func, Mode, Bin),
                 io:format("~10w ~6w ~6.2fms ±~4.2fms #~.2w gc included~n",
                           [Name, Mode, Mean/1000, Stddev/1000, N])
         end,
    io:format("----------------------~n"),
    Tokens = {tokens, fun(Str) -> str:tokens(Str, "\n") end},
    [Do(Name,Fun,Mode) || {Name,Fun} <- [Tokens], Mode <- [list, binary]],
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

test(Line, Func, Str, Args, Res, Norm) ->
    %%io:format("~p: ~p ~w ~w~n",[Line, Func, Str, Args]),
    test_1(Line, Func, Str, [Str|norm(none,Args)], Res),
    %%io:format("~p: ~p bin ",[Line, Func]),
    test_1({Line,list}, Func, Str,
           [unicode:characters_to_list(Str)|norm(none,Args)], Res),
    Norm andalso
        test_1({Line,clist}, Func, Str,
               [unicode:characters_to_nfc_list(Str)|norm(nfc,Args)], Res),
    Norm andalso
        test_1({Line,dlist}, Func, Str,
               [unicode:characters_to_nfd_list(Str)|norm(nfd,Args)], Res),
    test_1({Line,bin}, Func, Str,
           [unicode:characters_to_binary(Str)|norm(none, Args)], Res),
    Norm andalso
        test_1({Line,cbin}, Func, Str,
               [unicode:characters_to_nfc_binary(Str)|norm(nfc,Args)], Res),
    Norm andalso
        test_1({Line,dbin}, Func, Str,
               [unicode:characters_to_nfd_binary(Str)|norm(nfd,Args)], Res),
    %%io:format("~n",[]),
    ok.

test_1(Line, Func, Str, Args, Exp) ->
    try
        Res = apply(str, Func, Args),
        check_types(Line, Func, Args, Res),
        case res(Res, Exp) of
            true -> ok;
            {Res1,Exp1} ->
                io:format("~p:~p: ~ts~w =>~n  :~ts~w:~ts~w~n",
                          [Func,Line, Str,Str, Res1,Res1, Exp1,Exp1]),
                exit({error, Func})
        end
    catch
        error:Exp ->
            ok;
        error:Reason ->
            io:format("~p:~p: Crash ~p ~p~n",
                      [?MODULE,Line, Reason, erlang:get_stacktrace()]),
            exit({error, Func})
    end.

norm(Type, Args) ->
    Norm = case Type of
               nfc -> fun unicode:characters_to_nfc_list/1;
               nfd -> fun unicode:characters_to_nfd_list/1;
               none -> fun(Str) -> Str end
           end,
    lists:map(fun({norm,Str}) -> Norm(Str);
                 (Other) -> Other
              end, Args).


res(Str, Exp) when (is_list(Str) orelse is_binary(Str)), is_list(Exp) ->
    A = unicode:characters_to_nfc_list(Str),
    B = unicode:characters_to_nfc_list(Exp),
    A==B orelse {A,B};
res(What, {Fun, Exp}) when is_function(Fun) ->
    Fun(What) == Exp orelse {Fun(What), Exp};
res(Int, Exp) ->
    Int == Exp orelse {Int, Exp}.


check_types(_Line, _Func, _Str, Res)
  when is_integer(Res); is_boolean(Res); Res =:= nomatch ->
    %% length or equal
    ok;
check_types(Line, Func, [S1,S2], Res)
  when Func =:= concat ->
    case check_types_1(type(S1),type(S2)) of
        ok ->
            case check_types_1(type(S1),type(Res)) of
                ok -> ok;
                {T1,T2} ->
                    io:format("Failed: ~p ~p ~p ~p~n",[Line, Func, T1, T2]),
                    io:format("  ~p ~p  => ~p~n", [S1, S2, Res]),
                    error
            end;
        _ -> ok
    end;
check_types(Line, Func, [Str|_], Res)  ->
    AddList = fun(mixed) -> mixed;
                 ({list,{list,_}}) -> {list, deep};
                 (R) ->
                      case lists:member(Func, [tokens, split]) of
                          true -> {list, R};
                          false -> R
                      end
              end,
    try needs_check(Func) andalso (ok = check_types_1(AddList(type(Str)), type(Res))) of
        ok -> ok;
        false -> ok
    catch _:{badmatch, {T1,T2}} ->
            io:format("Failed: ~p ~p: ~p ~p~n",[Line, Func, T1, T2]),
            io:format("  ~p  => ~p~n", [Str, Res]),
            error;
          _:Reason ->
            io:format("Crash: ~p in~n ~p~n",[Reason, erlang:get_stacktrace()]),
            io:format("Failed: ~p ~p: ~p => ~p~n", [Line, Func, Str, Res]),
            exit({Reason, erlang:get_stacktrace()})
    end.

check_types_1(T, T) ->
    ok;
check_types_1(Str, Res)
  when is_binary(Str), is_binary(Res) ->
    ok;
check_types_1({list, _},{list, undefined}) ->
    ok;
check_types_1({list, _},{list, codepoints}) ->
    ok;
check_types_1({list, _},{list, {list, codepoints}}) ->
    ok;
check_types_1({list, {list, _}},{list, {list, codepoints}}) ->
    ok;
check_types_1(mixed,_) ->
    ok;
check_types_1({list, binary}, binary) ->
    ok;
check_types_1(T1,T2) ->
    {T1,T2}.

type(Bin) when is_binary(Bin) ->
    binary;
type([]) ->
    {list, undefined};
type(List) when is_list(List) ->
    Deep = fun(L) when is_list(L) ->
                   lists:any(fun(C) -> is_list(C) orelse is_binary(C) end, L);
              (_) -> false
           end,
    case all(fun(C) -> not is_binary(C) end, List) of
        true ->
            case all(fun(C) -> is_integer(C) end, List) of
                true -> {list, codepoints};
                false ->
                    case [deep || L <- List, Deep(L)] of
                        [] -> {list, {list, codepoints}};
                        _ -> {list, deep}
                    end
            end;
        false ->
            case all(fun(C) -> is_binary(C) end, List) of
                true -> {list, binary};
                false -> mixed
            end
    end;
type(Other) ->
    {other, Other}.

all(_Check, []) ->
    true;
all(Check, [H|T]) ->
    Check(H) andalso all(Check,T);
all(Check, Bin) when is_binary(Bin) ->
    Check(Bin).

needs_check(reverse) -> false;
needs_check(pad) -> false;
needs_check(replace) -> false;
needs_check(_) -> true.

%%%% Timer stuff

time_func(Fun, Mode, Bin) ->
    timer:sleep(100), %% Let emulator catch up and clean things before test runs
    Self = self(),
    Pid = spawn_link(fun() ->
                             Str = mode(Mode, Bin),
                             Self ! {self(),time_func(0,0,0, Fun, Str, undefined)}
                     end),
    receive {Pid,Msg} -> Msg end.

time_func(N,Sum,SumSq, Fun, Str, _) when N < 50 ->
    {Time, Res} = timer:tc(fun() -> Fun(Str) end),
    time_func(N+1,Sum+Time,SumSq+Time*Time, Fun, Str, Res);
time_func(N,Sum,SumSq, _, _, Res) ->
    Mean = round(Sum / N),
    Stdev = round(math:sqrt((SumSq - (Sum*Sum/N))/(N - 1))),
    {N, Mean, Stdev, Res}.

mode(binary, Bin) -> Bin;
mode(list, Bin) -> unicode:characters_to_list(Bin).
