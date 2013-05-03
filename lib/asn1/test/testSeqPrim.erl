%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testSeqPrim).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq',{bool, boolCon, boolPri, boolApp, boolExpCon, boolExpPri, boolExpApp}).
-record('Empty',{}).

main(Rules) ->
    roundtrip(Rules, 'Seq', #'Seq'{bool = true,
				   boolCon = true,
				   boolPri = true,
				   boolApp = true,
				   boolExpCon = true,
				   boolExpPri = true,
				   boolExpApp = true}),
    
    roundtrip(Rules, 'Seq', #'Seq'{bool = false,
				   boolCon = false,
				   boolPri = false,
				   boolApp = false,
				   boolExpCon = false,
				   boolExpPri = false,
				   boolExpApp = false}),
    roundtrip(Rules, 'Seq', #'Seq'{bool = false,
				   boolCon = true,
				   boolPri = false,
				   boolApp = true,
				   boolExpCon = false,
				   boolExpPri = true,
				   boolExpApp = false}),

    roundtrip(Rules, 'Empty', #'Empty'{}),

    roundtrip(Rules, 'SeqEnum', {'SeqEnum',true,42}),
    roundtrip(Rules, 'SeqEnumExt', {'SeqEnumExt',true,27}),
    roundtrip(Rules, 'SeqEnumExt', {'SeqEnumExt',false,21}),

    ok.

roundtrip(Rules, T, V) ->
    M = 'SeqPrim',
    {ok,E} = asn1_wrapper:encode(M, T, V),
    {ok,V} = asn1_wrapper:decode(M, T, E),
    verify(asn1_wrapper:erule(Rules), V, E),
    ok.

verify(per, {'SeqEnum',true,42}, [16#A8]) -> ok;
verify(per, {'SeqEnumExt',true,27}, [16#6C]) -> ok;
verify(per, {'SeqEnumExt',false,21}, [16#80,16#A8]) -> ok;
verify(per, #'Empty'{}, [0]) -> ok;
verify(per, #'Seq'{}, _) -> ok;
verify(ber, _, _) -> ok.
