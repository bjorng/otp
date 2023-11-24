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
    Bytes1 = roundtrip(M, 'F', FMsg),
    {ok,IncFMsg} = M:decode_F_fb_incomplete(Bytes1),
    decode_parts('F', IncFMsg),
    {ok,IncF2Msg} = M:decode_F_fb_exclusive2(Bytes1),
    decode_parts('F2', IncF2Msg),

    DMsg = msg('D'),
    Bytes2 = roundtrip(M, 'D', DMsg),
    {ok,IncDMsg} = M:decode_D_incomplete(Bytes2),
    decode_parts('D', IncDMsg),

    F3Msg = msg('F3'),
    BytesF3 = roundtrip(M, 'F', F3Msg),
    {ok,IncF3Msg} = M:decode_F_fb_exclusive3(BytesF3),
    decode_parts('F3', IncF3Msg),

    EMsg = msg('E'),
    Bytes4 = roundtrip(M, 'E', EMsg),
    {ok,IncEMsg} = M:decode_E_b_incomplete(Bytes4),
    decode_parts('E', IncEMsg),

    ok.

test_PartialDecSeq2() ->
    M = 'PartialDecSeq2',

    AMsg = msg('A'),
    Bytes1 = roundtrip(M, 'A', AMsg),
    {ok,IncAMsg} = M:decode_A_c_b_incomplete(Bytes1),
    decode_parts('A', IncAMsg),

    SMsg = {'S',true,false},
    BextMsg = {c,SMsg},
    Bytes2 = roundtrip(M, 'Bext', BextMsg),

    {ok,IncBextMsg1} = M:decode_Bext_c_incomplete(Bytes2),
    {c,{'Bext_c',Undec1}} = IncBextMsg1,
    {ok,SMsg} = M:decode_part('Bext_c', Undec1),

    {ok,IncBextMsg2} = M:decode_Bext_c_b_incomplete(Bytes2),
    {c,{'S',true,{'S_b',Undec2}}} = IncBextMsg2,
    {ok,false} = M:decode_part('S_b', Undec2),

    test_PartialDeqSeq2_SeqChoice(),

    ok.

test_PartialDeqSeq2_SeqChoice() ->
    M = 'PartialDecSeq2',
    T = 'SeqChoice',

    SeqChoiceMsg1 = {'SeqChoice',{b,true},<<"abc">>},
    Bytes1 = roundtrip(M, T, SeqChoiceMsg1),

    {ok,EncMsg1_1} = M:decode_SeqChoice_c_b_d_incomplete(Bytes1),
    {'SeqChoice',{b,{'SeqChoice_c_b',UndecBool}},
     {'SeqChoice_d',UndecOS}} = EncMsg1_1,
    {ok,true} = M:decode_part('SeqChoice_c_b', UndecBool),
    {ok,<<"abc">>} = M:decode_part('SeqChoice_d', UndecOS),

    {ok,EncMsg1_2} = M:decode_SeqChoice_c_bis_incomplete(Bytes1),
    {'SeqChoice',{b,{'SeqChoice_c_b',UndecBool}},<<"abc">>} = EncMsg1_2,
    {ok,true} = M:decode_part('SeqChoice_c_b', UndecBool),

    SeqChoiceMsg2 = {'SeqChoice',{i,42},<<"cde">>},
    Bytes2 = roundtrip(M, T, SeqChoiceMsg2),
    {ok,EncMsg2_1} = M:decode_SeqChoice_c_bis_incomplete(Bytes2),
    {'SeqChoice',{i,{'SeqChoice_c_i',UndecInt}},<<"cde">>} = EncMsg2_1,
    {ok,42} = M:decode_part('SeqChoice_c_i', UndecInt),

    SeqChoiceMsg3 = {'SeqChoice',{s,"xyz"},<<"fgh">>},
    Bytes3 = roundtrip(M, T, SeqChoiceMsg3),
    {ok,EncMsg3_1} = M:decode_SeqChoice_c_bis_incomplete(Bytes3),
    {'SeqChoice',{s,{'SeqChoice_c_s',UndecStr}},<<"fgh">>} = EncMsg3_1,
    {ok,"xyz"} = M:decode_part('SeqChoice_c_s', UndecStr),

    ok.

test_PartialDecSeq3() ->
    M = 'PartialDecSeq3',

    MsgS1_1 = msg('S1_1'),
    Bytes1 = roundtrip(M, 'S1', MsgS1_1),
    {ok,IncFMsg1} = M:decode_S1_incomplete(Bytes1),
    decode_parts('S1_1', IncFMsg1),

    {ok,IncBMsg1} = M:decode_S1_b_incomplete(Bytes1),
    MsgS1_1 = decode_parts('S1_b', IncBMsg1),

    MsgS1_2 = msg('S1_2'),
    Bytes2 = roundtrip(M, 'S1', MsgS1_2),
    {ok,IncFMsg2} = M:decode_S1_incomplete(Bytes2),
    decode_parts('S1_2', IncFMsg2),

    MsgS3 = msg('S3'),
    Bytes3 = roundtrip(M, 'S3', MsgS3),
    {ok,IncSMsg} = M:decode_S3_second(Bytes3),
    decode_parts('S3', IncSMsg),

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

    {ok,Dec1} = Mod:decode_BasicOCSPResponse_signature_undec(Enc),
    {Type,_,_,{SignTag,SignUndec},_} = Dec1,
    {ok,<<"signature">>} = Mod:decode_part(SignTag, SignUndec),

    {ok,Dec2} = Mod:decode_BasicOCSPResponse_certs_undec(Enc),
    {Type,_,_,_,{CertTag,CertUndec}} = Dec2,
    {ok,[]} = Mod:decode_part(CertTag, CertUndec),

    ok.

decode_parts('F',PartDecMsg) ->
    {fb,{'E',35,{NameE_b,ListBinE_b},false,{NameE_d,BinE_d}}} = PartDecMsg,
    {ok,[{'D',3,true}|_]} = 'PartialDecSeq':decode_part(NameE_b,ListBinE_b),
    {ok,{'D',3,true}} = 'PartialDecSeq':decode_part(NameE_b,
							  hd(ListBinE_b)),
    {ok,{da,[{'A',16,{'D',17,true}}]}} =
	'PartialDecSeq':decode_part(NameE_d,BinE_d),
    ok;
decode_parts('F2',PartDecMsg) ->
    {fb,{'E',35,{E_bkey,E_b},false,{da,{E_d_akey,E_d_a}}}} = PartDecMsg,
    {ok,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    {ok,[{'A',16,{'D',17,true}}]} = 'PartialDecSeq':decode_part(E_d_akey,E_d_a);

decode_parts('F3',PartDecMsg) ->
    {fb,{'E',10,{E_bkey,E_b},false,{dc,{'E_d_dc',13,true,{E_d_dc_dcckey,E_d_dc_dcc}}}}} = PartDecMsg,
    {ok,[{'D',11,true},{'D',12,false}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    {ok,{'E_d_dc_dcc',14,15}} = 'PartialDecSeq':decode_part(E_d_dc_dcckey,E_d_dc_dcc);


decode_parts('D',PartDecMsg) ->
    {'D',{NameD_a,BinD_a},true} = PartDecMsg,
    {ok,123} = 'PartialDecSeq':decode_part(NameD_a,BinD_a),
    ok;
decode_parts('A',PartDecMsg) ->
    {'A',12,{c,{'S',true,false}},{b,{NameA_c_b,BinA_c_b}}} = PartDecMsg,
    {ok,{'A_c_b',false,false}} =
	'PartialDecSeq2':decode_part(NameA_c_b,BinA_c_b),
    ok;
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
    ok;
decode_parts('S1_1',PartDecMsg) ->
    {'S1',14,{'S2',false,12,{NameS2c,BinS2c}},
	   {_,{NameS1c_a,ListBinS1c_a}},{NameS1d,BinS1d}} = PartDecMsg,
    {ok,[{'S3',10,"PrintableString",<<"OCTETSTRING">>,
		[one,two,three,four]}|_Rest1]} = 
	'PartialDecSeq3':decode_part(NameS2c,BinS2c),
    {ok,[{'S3',10,"PrintableString",<<"OCTETSTRING">>,
		[one,two,three,four]}|_Rest2]} = 
	'PartialDecSeq3':decode_part(NameS1c_a,ListBinS1c_a),
    {ok,{'S3',10,"PrintableString",<<"OCTETSTRING">>,
	       [one,two,three,four]}} =
	'PartialDecSeq3':decode_part(NameS1c_a,hd(ListBinS1c_a)),
    {ok,[{'Name',"Hans","HCA","Andersen"}|_Rest3]} =
	'PartialDecSeq3':decode_part(NameS1d,BinS1d),
    ok;
decode_parts('S1_2',PartDecMsg) ->
    {'S1',14,{'S2',false,12,_S2c},S1c_b,{NameS1d,BinS1d}} = PartDecMsg,
    {b,{'C1_b',11,true,
	      {'S4',{'Name',"Hans","HCA","Andersen"},"MSc"}}}=S1c_b,
    {ok,[{'Name',"Hans","HCA","Andersen"}|_Rest3]} =
	'PartialDecSeq3':decode_part(NameS1d,BinS1d),
    ok;
decode_parts('S1_b', PartDecMsg) ->
    {'S1',14,{PartName,PartBin},Choice,Names} = PartDecMsg,
    {ok,S2} = 'PartialDecSeq3':decode_part(PartName, PartBin),
    {'S1',14,S2,Choice,Names};
decode_parts('S3', PartDecMsg) ->
    {'S3',10,{'S3_second',Undecoded},<<"OCTETSTRING">>,[one,two,three,four]} = PartDecMsg,
    {ok,"PrintableString"} = 'PartialDecSeq3':decode_part('S3_second', Undecoded),
    ok;
decode_parts('E', PartDecMsg) ->
    {'E',35,{'E_b',Parts},false,_} = PartDecMsg,
    DMany = msg('D_many'),
    {ok,DMany} = 'PartialDecSeq':decode_part('E_b', Parts),
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
