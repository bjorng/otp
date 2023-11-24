%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%
-module(test_partial_incomplete_decode).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

test(Config) ->
    test_PartialDecSeq(),
    test_PartialDecSeq2(),
    test_PartialDecSeq3(),
    test_MyHTTPMsg(),
    test_megaco(Config),
    test_OCSP(),
    ok.

test_PartialDecSeq() ->
    M = 'PartialDecSeq',

    FMsg = msg('F'),
    test_exclusive(fun M:decode_F_fb_incomplete/1, 'F', FMsg),

    DMsg = msg('D'),
    test_exclusive(fun M:decode_D_incomplete/1, 'D', DMsg),

    F3Msg = msg('F3'),
    %% test_exclusive(fun M:decode_F_fb_exclusive3/1, 'F', F3Msg),
    BytesF3 = roundtrip(M, 'F', F3Msg),
    {ok,IncF3Msg} = M:decode_F_fb_exclusive3(BytesF3),
    decode_parts('F3', IncF3Msg),

    EMsg = msg('E'),
    test_exclusive(fun M:decode_E_b_incomplete/1, 'E', EMsg),

    ok.

test_PartialDecSeq2() ->
    M = 'PartialDecSeq2',

    AMsg = msg('A'),
    test_exclusive(fun M:decode_A_c_b_incomplete/1, 'A', AMsg),

    SMsg = {'S',true,false},
    BextMsg = {c,SMsg},

    test_exclusive(fun M:decode_Bext_c_incomplete/1, 'Bext', BextMsg),
    test_exclusive(fun M:decode_Bext_c_b_incomplete/1, 'Bext', BextMsg),

    T = 'SeqChoice',

    SeqChoiceMsg1 = {'SeqChoice',{b,true},<<"abc">>},
    test_exclusive(fun M:decode_SeqChoice_c_b_d_incomplete/1, T, SeqChoiceMsg1),

    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg1),

    SeqChoiceMsg2 = {'SeqChoice',{i,42},<<"cde">>},
    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg2),

    SeqChoiceMsg3 = {'SeqChoice',{s,"xyz"},<<"fgh">>},
    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg3),

    ok.

test_PartialDecSeq3() ->
    M = 'PartialDecSeq3',

    MsgS1_1 = msg('S1_1'),
    test_exclusive(fun M:decode_S1_incomplete/1, 'S1', MsgS1_1),
    test_exclusive(fun M:decode_S1_b_incomplete/1, 'S1', MsgS1_1),

    MsgS1_2 = msg('S1_2'),
    test_exclusive(fun M:decode_S1_incomplete/1, 'S1', MsgS1_2),

    MsgS3 = msg('S3'),
    test_exclusive(fun M:decode_S3_second/1, 'S3', MsgS3),

    ok.

test_MyHTTPMsg() ->
    MyHTTPMsg = msg('GetRequest'),
    Bytes1 = roundtrip('PartialDecMyHTTP', 'GetRequest', MyHTTPMsg),
    {ok,IncFMsg4} = 'PartialDecMyHTTP':decode_GetRequest_incomplete(Bytes1),
    decode_parts('GetRequest', IncFMsg4),

    ok.

test_megaco(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = filelib:wildcard(filename:join([DataDir,megacomessages,"*.val"])),
    Mod = 'MEDIA-GATEWAY-CONTROL',
    lists:foreach(fun(File) ->
			  {ok,Bin} = file:read_file(File),
			  V = binary_to_term(Bin),
			  T = element(1, V),
			  Enc = roundtrip(Mod, T, V),
			  exclusive_decode(Enc, File)
		  end, Files).

exclusive_decode(Bin,F) ->
    Mod='MEDIA-GATEWAY-CONTROL',
    io:format("Encoding message: ~p~n",[F]),
    {ok,{_,_,{_,_VsnNo,{MsgMidKey,MsgMid},{MsgMBodyKey,MsgMBody}}}}=
	Mod:decode_MegacoMessage_exclusive(Bin),
    {ok,_} = Mod:decode_part(MsgMidKey,MsgMid),
    {ok,_} = Mod:decode_part(MsgMBodyKey,MsgMBody),
    ok.

test_OCSP() ->
    Mod = 'OCSP-2013-88',

    ResponseData = {'ResponseData',
                    0,                          %Version
                    {byKey,<<"key hash">>},
                    "factory",
                    [],
                    asn1_NOVALUE},

    Type = 'BasicOCSPResponse',
    Msg = {Type,
           ResponseData,
           {'AlgorithmIdentifier',Mod:'id-pkix-ocsp-basic'(),<<>>},
           <<"signature">>,
           []},
    {ok,Enc} = Mod:encode(Type, Msg),

    %% test_exclusive(fun Mod:decode_BasicOCSPResponse_signature_undec/1, Type, Msg),
    {ok,Dec1} = Mod:decode_BasicOCSPResponse_signature_undec(Enc),
    {Type,_,_,{SignTag,SignUndec},_} = Dec1,
    {ok,<<"signature">>} = Mod:decode_part(SignTag, SignUndec),

    %% test_exclusive(fun Mod:decode_BasicOCSPResponse_certs_undec/1, Type, Msg),
    {ok,Dec2} = Mod:decode_BasicOCSPResponse_certs_undec(Enc),
    {Type,_,_,_,{CertTag,CertUndec}} = Dec2,
    {ok,[]} = Mod:decode_part(CertTag, CertUndec),

    ok.

decode_parts('F3',PartDecMsg) ->
    {fb,{'E',10,{E_bkey,E_b},false,{dc,{'E_d_dc',13,true,{E_d_dc_dcckey,E_d_dc_dcc}}}}} = PartDecMsg,
    {ok,[{'D',11,true},{'D',12,false}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    {ok,{'E_d_dc_dcc',14,15}} = 'PartialDecSeq':decode_part(E_d_dc_dcckey,E_d_dc_dcc);
decode_parts('GetRequest',PartDecMsg) ->
    {'GetRequest',true,false,
	   {'AcceptTypes',[html,'plain-text',gif,jpeg],
	    {NameAcceptTypes_others,ListBinAcceptTypes_others}},
	   "IamfineThankYOu"} = PartDecMsg,
    {ok,["hell","othe","reho","peyo","uare","fine"]} =
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       ListBinAcceptTypes_others),
    {ok,"hell"} =
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       hd(ListBinAcceptTypes_others)),
    ok.

msg('E') ->
    {'E',35,msg('D_many'),false,{da,[{'A',16,{'D',17,true}}]}};

msg('D_many') ->
    [{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},
     {'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}];

msg('F') ->
    {fb,msg('E')};

msg('F3') ->
    {fb,{'E',10,[{'D',11,true},{'D',12,false}],false,{dc,{'E_d_dc',13,true,{'E_d_dc_dcc',14,15}}}}};

msg('D') ->
    {'D',123,true};

msg('A') ->
    {'A',12,{c,{'S',true,false}},{b,{'A_c_b',false,false}}};

msg('GetRequest') ->
    {'GetRequest',true,false,
     {'AcceptTypes',[html,'plain-text',gif,jpeg],
      ["hell","othe","reho","peyo","uare","fine"]},
     "IamfineThankYOu"};

msg('S1_1') ->
    {'S1',14,msg('S2'),msg('C1_a'),msg('SO1')};
msg('S1_2') ->
    {'S1',14,msg('S2'),msg('C1_b'),msg('SO1')};
msg('S2') ->
    {'S2',false,12,[msg('S3'),msg('S3'),msg('S3')]};
msg('C1_a') ->
    {a,[msg('S3'),msg('S3'),msg('S3')]};
msg('C1_b') ->
    {b,{'C1_b',11,true,msg('S4')}};
msg('S3') ->
    {'S3',10,"PrintableString",<<"OCTETSTRING">>,[one,two,three,four]};
msg('S4') ->
    {'S4',msg('Name'),"MSc"};
msg('SO1') ->
    [msg('Name'),msg('Name'),msg('Name')];
msg('Name') ->
    {'Name',"Hans","HCA","Andersen"}.

roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip_enc(M, T, V).

test_exclusive(DecodeFun, Type, Msg) ->
    {module,Mod} = erlang:fun_info(DecodeFun, module),
    Encoded = roundtrip(Mod, Type, Msg),
    case DecodeFun(Encoded) of
        {ok,Msg} ->
            error({should_be_different,Msg});
        {ok,Decoded} ->
            case dec_parts(Decoded, Mod) of
                Msg ->
                    ok;
                OtherMsg ->
                    io:format("""
                              Partial decoding:
                              ~p

                              Expected:
                              ~p

                              Got:
                              ~p
                              """, [Decoded,Msg,OtherMsg]),
                    error(full_and_partial_decode_differ)
            end
    end.

dec_parts({Name,Bin}, Mod) when is_atom(Name), is_binary(Bin) ->
    {ok,Dec} = Mod:decode_part(Name, Bin),
    Dec;
dec_parts({Name,[_|_]=Parts}, Mod) when is_atom(Name) ->
    case lists:all(fun erlang:is_binary/1, Parts) of
        true ->
            [begin
                 {ok,Dec} = Mod:decode_part(Name, Bin),
                 Dec
             end || Bin <- Parts];
        false ->
            {Name,[dec_parts(E, Mod) || E <- Parts]}
    end;
dec_parts(Tuple0, Mod) when is_tuple(Tuple0) ->
    Tuple = [dec_parts(E, Mod) || E <- tuple_to_list(Tuple0)],
    list_to_tuple(Tuple);
dec_parts(List, Mod) when is_list(List) ->
    [dec_parts(E, Mod) || E <- List];
dec_parts(Other, _Mod) ->
    Other.
