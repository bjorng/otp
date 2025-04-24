%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(pubkey_cert_records).
-moduledoc false.

-define(_PKCS_FRAME_HRL_, true).
-include("public_key_internal.hrl").

%% Superseded by SingleAttribute.
-record('AttributeTypeAndValue', {
  type,
  value
}).

-record('OTPSubjectPublicKeyInfo',
        {
         algorithm,       % #'PublicKeyAlgorithm'{}
         subjectPublicKey % binary()
        }).

%% Identifiers not present in modern specs.

-define('characteristic-two-field', {1,2,840,10045,1,2}).
-define('prime-field', {1,2,840,10045,1,1}).

-define('brainpoolP512t1', {1,3,36,3,3,2,8,1,1,14}).
-define('brainpoolP512r1', {1,3,36,3,3,2,8,1,1,13}).
-define('brainpoolP384t1', {1,3,36,3,3,2,8,1,1,12}).
-define('brainpoolP384r1', {1,3,36,3,3,2,8,1,1,11}).
-define('brainpoolP320t1', {1,3,36,3,3,2,8,1,1,10}).
-define('brainpoolP320r1', {1,3,36,3,3,2,8,1,1,9}).
-define('brainpoolP256t1', {1,3,36,3,3,2,8,1,1,8}).
-define('brainpoolP256r1', {1,3,36,3,3,2,8,1,1,7}).
-define('brainpoolP224t1', {1,3,36,3,3,2,8,1,1,6}).
-define('brainpoolP224r1', {1,3,36,3,3,2,8,1,1,5}).
-define('brainpoolP192t1', {1,3,36,3,3,2,8,1,1,4}).
-define('brainpoolP192r1', {1,3,36,3,3,2,8,1,1,3}).
-define('brainpoolP160t1', {1,3,36,3,3,2,8,1,1,2}).
-define('brainpoolP160r1', {1,3,36,3,3,2,8,1,1,1}).
-define('secp224k1', {1,3,132,0,32}).
-define('secp192k1', {1,3,132,0,31}).
-define('secp160r2', {1,3,132,0,30}).
-define('secp128r2', {1,3,132,0,29}).
-define('secp128r1', {1,3,132,0,28}).
-define('sect193r2', {1,3,132,0,25}).
-define('sect193r1', {1,3,132,0,24}).
-define('sect131r2', {1,3,132,0,23}).
-define('sect131r1', {1,3,132,0,22}).
-define('secp256k1', {1,3,132,0,10}).
-define('secp160k1', {1,3,132,0,9}).
-define('secp160r1', {1,3,132,0,8}).
-define('secp112r2', {1,3,132,0,7}).
-define('secp112r1', {1,3,132,0,6}).
-define('sect113r2', {1,3,132,0,5}).
-define('sect113r1', {1,3,132,0,4}).
-define('sect239k1', {1,3,132,0,3}).
-define('sect163r1', {1,3,132,0,2}).

-export([decode_cert/1, transform/2, supportedPublicKeyAlgorithms/1,
	 supportedCurvesTypes/1, namedCurves/1]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec decode_cert(DerCert::binary()) -> {ok, #'OTPCertificate'{}}.
%%
%% Description: Recursively decodes a Certificate. 
%%-------------------------------------------------------------------- 
decode_cert(DerCert) ->
    {ok, Cert} = 'OTP-PKIX':decode('OTPCertificate', DerCert),
    #'OTPCertificate'{tbsCertificate = TBS, signatureAlgorithm=SA} = Cert,
    {ok, Cert#'OTPCertificate'{tbsCertificate = decode_tbs(TBS),
                               signatureAlgorithm = setelement(1, SA, 'SignatureAlgorithm')
                              }}.

%%--------------------------------------------------------------------
-spec transform(term(), encode | decode) ->term().
%%
%% Description: Transforms between encoded and decode otp formatted
%% certificate parts.
%%
%% Note that this function operates on raw data that has not gone
%% through the pubkey_translation module. Thus does the same
%% backwards compatibility translation done in pubkey_translation.
%%--------------------------------------------------------------------

transform(#'OTPCertificate'{tbsCertificate = TBS, signatureAlgorithm=SA} = Cert, encode) ->
    Cert#'OTPCertificate'{tbsCertificate=encode_tbs(TBS),
                          signatureAlgorithm = setelement(1, SA, 'OTPCertificate_SignatureAlgorithm')};
transform(#'OTPCertificate'{tbsCertificate = TBS, signatureAlgorithm=SA} = Cert, decode) ->
    Cert#'OTPCertificate'{tbsCertificate=decode_tbs(TBS),
                          signatureAlgorithm = setelement(1, SA, 'SignatureAlgorithm')};
transform(#'OTPTBSCertificate'{}= TBS, encode) ->
    encode_tbs(TBS);
transform(#'OTPTBSCertificate'{}= TBS, decode) ->
    decode_tbs(TBS);
transform(#'SingleAttribute'{type=Id,value=Value0}, decode) ->
    case {Id, Value0} of
        {?'id-at-countryName', {_,String}} ->
            #'AttributeTypeAndValue'{type=Id, value=String};
        {?'id-emailAddress', {_,String}} ->
            #'AttributeTypeAndValue'{type=Id, value=String};
        {_, _} ->
            #'AttributeTypeAndValue'{type=Id, value=Value0}
    end;
transform(#'AttributeTypeAndValue'{type=Id, value=Value0}, encode) ->
    case Id of
        ?'id-at-countryName' ->
            #'SingleAttribute'{type=Id, value={correct, Value0}};
        ?'id-emailAddress' ->
            #'SingleAttribute'{type=Id, value={correct, Value0}};
        _ ->
            #'SingleAttribute'{type=Id,value=Value0}
    end;
transform(AKI = #'AuthorityKeyIdentifier'{authorityCertIssuer=ACI},Func) ->
    AKI#'AuthorityKeyIdentifier'{authorityCertIssuer=transform(ACI,Func)};
transform(List = [{directoryName, _}],Func) ->
    [{directoryName, transform(Value,Func)} || {directoryName, Value} <- List];
transform({directoryName, Value},Func) ->
    {directoryName, transform(Value,Func)};
transform({rdnSequence, SeqList},Func) when is_list(SeqList) ->
    {rdnSequence, 
     lists:map(fun(Seq) -> 
		       lists:map(fun(Element) -> transform(Element,Func) end, Seq)
	       end, SeqList)};
transform(#'NameConstraints'{permittedSubtrees=Permitted, excludedSubtrees=Excluded}, Func) ->
    #'NameConstraints'{permittedSubtrees=transform_sub_tree(Permitted,Func),
		       excludedSubtrees=transform_sub_tree(Excluded,Func)};
	  
transform(Other,_) ->
    Other.

%%--------------------------------------------------------------------
-spec supportedPublicKeyAlgorithms(Oid::tuple()) -> public_key:asn1_type().
%%
%% Description: Returns the public key type for an algorithm
%% identifier tuple as found in SubjectPublicKeyInfo.
%%
%%--------------------------------------------------------------------
supportedPublicKeyAlgorithms(?'rsaEncryption') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-dsa') -> 'DSAPublicKey';
supportedPublicKeyAlgorithms(?'dhpublicnumber') -> 'DHPublicKey';
supportedPublicKeyAlgorithms(?'id-keyExchangeAlgorithm') -> 'KEA-PublicKey';
supportedPublicKeyAlgorithms(?'id-ecPublicKey') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-RSASSA-PSS') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-Ed25519') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-Ed448') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-X25519') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-X448') -> 'ECPoint'.

supportedCurvesTypes(?'characteristic-two-field') -> characteristic_two_field;
supportedCurvesTypes(?'prime-field') -> prime_field;
supportedCurvesTypes(?'id-edwards-curve-algs') -> edwards_curve.

namedCurves(?'sect571r1') -> sect571r1;
namedCurves(?'sect571k1') -> sect571k1;
namedCurves(?'sect409r1') -> sect409r1;
namedCurves(?'sect409k1') -> sect409k1;
namedCurves(?'secp521r1') -> secp521r1;
namedCurves(?'secp384r1') -> secp384r1;
namedCurves(?'secp224r1') -> secp224r1;
namedCurves(?'secp224k1') -> secp224k1;
namedCurves(?'secp192k1') -> secp192k1;
namedCurves(?'secp160r2') -> secp160r2;
namedCurves(?'secp128r2') -> secp128r2;
namedCurves(?'secp128r1') -> secp128r1;
namedCurves(?'sect233r1') -> sect233r1;
namedCurves(?'sect233k1') -> sect233k1;
namedCurves(?'sect193r2') -> sect193r2;
namedCurves(?'sect193r1') -> sect193r1;
namedCurves(?'sect131r2') -> sect131r2;
namedCurves(?'sect131r1') -> sect131r1;
namedCurves(?'sect283r1') -> sect283r1;
namedCurves(?'sect283k1') -> sect283k1;
namedCurves(?'sect163r2') -> sect163r2;
namedCurves(?'secp256k1') -> secp256k1;
namedCurves(?'secp160k1') -> secp160k1;
namedCurves(?'secp160r1') -> secp160r1;
namedCurves(?'secp112r2') -> secp112r2;
namedCurves(?'secp112r1') -> secp112r1;
namedCurves(?'sect113r2') -> sect113r2;
namedCurves(?'sect113r1') -> sect113r1;
namedCurves(?'sect239k1') -> sect239k1;
namedCurves(?'sect163r1') -> sect163r1;
namedCurves(?'sect163k1') -> sect163k1;
namedCurves(?'secp256r1') -> secp256r1;
namedCurves(?'secp192r1') -> secp192r1;
namedCurves(?'id-X25519') -> x25519;
namedCurves(?'id-X448') -> x448;
namedCurves(?'id-Ed25519') -> ed25519;
namedCurves(?'id-Ed448') -> ed448;
namedCurves(?'brainpoolP160r1') -> brainpoolP160r1;
namedCurves(?'brainpoolP160t1') -> brainpoolP160t1;
namedCurves(?'brainpoolP192r1') -> brainpoolP192r1;
namedCurves(?'brainpoolP192t1') -> brainpoolP192t1;
namedCurves(?'brainpoolP224r1') -> brainpoolP224r1;
namedCurves(?'brainpoolP224t1') -> brainpoolP224t1;
namedCurves(?'brainpoolP256r1') -> brainpoolP256r1;
namedCurves(?'brainpoolP256t1') -> brainpoolP256t1;
namedCurves(?'brainpoolP320r1') -> brainpoolP320r1;
namedCurves(?'brainpoolP320t1') -> brainpoolP320t1;
namedCurves(?'brainpoolP384r1') -> brainpoolP384r1;
namedCurves(?'brainpoolP384t1') -> brainpoolP384t1;
namedCurves(?'brainpoolP512r1') -> brainpoolP512r1;
namedCurves(?'brainpoolP512t1') -> brainpoolP512t1;
namedCurves(sect571r1) -> ?'sect571r1';
namedCurves(sect571k1) -> ?'sect571k1';
namedCurves(sect409r1) -> ?'sect409r1';
namedCurves(sect409k1) -> ?'sect409k1';
namedCurves(secp521r1) -> ?'secp521r1';
namedCurves(secp384r1) -> ?'secp384r1';
namedCurves(secp224r1) -> ?'secp224r1';
namedCurves(secp224k1) -> ?'secp224k1';
namedCurves(secp192k1) -> ?'secp192k1';
namedCurves(secp160r2) -> ?'secp160r2';
namedCurves(secp128r2) -> ?'secp128r2';
namedCurves(secp128r1) -> ?'secp128r1';
namedCurves(sect233r1) -> ?'sect233r1';
namedCurves(sect233k1) -> ?'sect233k1';
namedCurves(sect193r2) -> ?'sect193r2';
namedCurves(sect193r1) -> ?'sect193r1';
namedCurves(sect131r2) -> ?'sect131r2';
namedCurves(sect131r1) -> ?'sect131r1';
namedCurves(sect283r1) -> ?'sect283r1';
namedCurves(sect283k1) -> ?'sect283k1';
namedCurves(sect163r2) -> ?'sect163r2';
namedCurves(secp256k1) -> ?'secp256k1';
namedCurves(secp160k1) -> ?'secp160k1';
namedCurves(secp160r1) -> ?'secp160r1';
namedCurves(secp112r2) -> ?'secp112r2';
namedCurves(secp112r1) -> ?'secp112r1';
namedCurves(sect113r2) -> ?'sect113r2';
namedCurves(sect113r1) -> ?'sect113r1';
namedCurves(sect239k1) -> ?'sect239k1';
namedCurves(sect163r1) -> ?'sect163r1';
namedCurves(sect163k1) -> ?'sect163k1';
namedCurves(secp256r1) -> ?'secp256r1';
namedCurves(secp192r1) -> ?'secp192r1';
namedCurves(x25519)    -> ?'id-X25519';
namedCurves(x448)      -> ?'id-X448';
namedCurves(ed25519)    -> ?'id-Ed25519';
namedCurves(ed448)      -> ?'id-Ed448';
namedCurves(brainpoolP160r1) -> ?'brainpoolP160r1';
namedCurves(brainpoolP160t1) -> ?'brainpoolP160t1';
namedCurves(brainpoolP192r1) -> ?'brainpoolP192r1';
namedCurves(brainpoolP192t1) -> ?'brainpoolP192t1';
namedCurves(brainpoolP224r1) -> ?'brainpoolP224r1';
namedCurves(brainpoolP224t1) -> ?'brainpoolP224t1';
namedCurves(brainpoolP256r1) -> ?'brainpoolP256r1';
namedCurves(brainpoolP256t1) -> ?'brainpoolP256t1';
namedCurves(brainpoolP320r1) -> ?'brainpoolP320r1';
namedCurves(brainpoolP320t1) -> ?'brainpoolP320t1';
namedCurves(brainpoolP384r1) -> ?'brainpoolP384r1';
namedCurves(brainpoolP384t1) -> ?'brainpoolP384t1';
namedCurves(brainpoolP512r1) -> ?'brainpoolP512r1';
namedCurves(brainpoolP512t1) -> ?'brainpoolP512t1'.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%% SubjectPublicKey

decode_supportedPublicKey(#'SubjectPublicKeyInfo'{algorithm=PA,
                                                  subjectPublicKey=SPK0}) ->
    #'SubjectPublicKeyInfo_algorithm'{algorithm=Algo,parameters=Params0} = PA,
    Type = supportedPublicKeyAlgorithms(Algo),
    SPK = case Type of
              'ECPoint' ->
                  #'ECPoint'{point = SPK0};
              _ ->
                  public_key:der_decode(Type, SPK0)
          end,
    Params = case Params0 of
                 #'DSA-Params'{p=P,q=Q,g=G} -> {params, #'Dss-Parms'{p=P,q=Q,g=G}};
                 _ -> Params0
             end,
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = SPK,
                               algorithm=#'PublicKeyAlgorithm'{algorithm=Algo,
                                                               parameters=Params}}.
encode_supportedPublicKey(#'OTPSubjectPublicKeyInfo'{
                             algorithm =
                                 #'PublicKeyAlgorithm'{algorithm=Algo,parameters = Params0},
                             subjectPublicKey = SPK0}) ->
    Type = supportedPublicKeyAlgorithms(Algo),
    SPK = case Type of
              'ECPoint' ->
                  SPK0#'ECPoint'.point;
              _ ->
                  public_key:der_encode(Type, SPK0)
          end,
    Params = case Params0 of
                 {params, #'Dss-Parms'{p=P,q=Q,g=G}} -> #'DSA-Params'{p=P,q=Q,g=G};
                 _ -> Params0
             end,
    PA = #'SubjectPublicKeyInfo_algorithm'{algorithm=Algo,parameters=Params},
    #'SubjectPublicKeyInfo'{subjectPublicKey = SPK, algorithm=PA}.

%%% Extensions

extension_id(?'id-ce-authorityKeyIdentifier') ->  'AuthorityKeyIdentifier';
extension_id(?'id-ce-subjectKeyIdentifier') ->    'SubjectKeyIdentifier';
extension_id(?'id-ce-keyUsage') -> 	          'KeyUsage';
extension_id(?'id-ce-privateKeyUsagePeriod') ->   'PrivateKeyUsagePeriod';
extension_id(?'id-ce-certificatePolicies') -> 	  'CertificatePolicies';
extension_id(?'id-ce-policyMappings') -> 	  'PolicyMappings';
extension_id(?'id-ce-subjectAltName') -> 	  'SubjectAltName';
extension_id(?'id-ce-issuerAltName') -> 	  'IssuerAltName';
extension_id(?'id-ce-subjectDirectoryAttributes') -> 	  'SubjectDirectoryAttributes';
extension_id(?'id-ce-basicConstraints' ) -> 	  'BasicConstraints';
extension_id(?'id-ce-nameConstraints') -> 	  'NameConstraints';
extension_id(?'id-ce-policyConstraints') -> 	  'PolicyConstraints';
extension_id(?'id-ce-extKeyUsage') -> 	          'ExtKeyUsageSyntax';
extension_id(?'id-ce-inhibitAnyPolicy') -> 	  'InhibitAnyPolicy';
extension_id(?'id-ce-freshestCRL') -> 	          'FreshestCRL';
extension_id(?'id-ce-issuingDistributionPoint') -> 'IssuingDistributionPoint';
%% Missing in public_key doc
extension_id(?'id-pe-authorityInfoAccess') -> 	  'AuthorityInfoAccessSyntax';
extension_id(?'id-pe-subjectInfoAccess') -> 	  'SubjectInfoAccessSyntax';
extension_id(?'id-ce-cRLNumber') -> 	          'CRLNumber';
extension_id(?'id-ce-deltaCRLIndicator') -> 	   'BaseCRLNumber';
extension_id(?'id-ce-cRLReasons') -> 	          'CRLReason';
extension_id(?'id-ce-certificateIssuer') -> 	  'CertificateIssuer';
extension_id(?'id-ce-holdInstructionCode') -> 	  'HoldInstructionCode';
extension_id(?'id-ce-invalidityDate') -> 	  'InvalidityDate';
extension_id(_) ->
    undefined.

decode_extensions(asn1_NOVALUE) ->
    asn1_NOVALUE;

decode_extensions(Exts) ->
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
                      ExtId = extension_id(Id),
		      case ExtId =/= undefined andalso
                          'PKIX1Implicit-2009':getdec_CertExtensions(Id)
                      of
			  false ->
                              Ext;
                          {asn1_OPENTYPE, _} ->
                              Ext;
                          DecodeExt when ExtId =:= 'CertificatePolicies',
                                         is_function(DecodeExt, 3) ->
                              %% Might need workaround to gracefully handle long user notices
                              try
                                  Value = DecodeExt('ExtnType', iolist_to_binary(Value0), dummy),
                                  Ext#'Extension'{extnValue=transform(Value,decode)}
                              catch exit:{_, {error,{asn1,bad_range}}} ->
                                      decode_otp_cert_polices(Ext, iolist_to_binary(Value0))
                              end;
			  DecodeExt when is_function(DecodeExt, 3) ->
                              %% Undocumented asn1 usage, but
                              %% currently the only way to decode
                              %% extensions.
                              Value = DecodeExt('ExtnType', iolist_to_binary(Value0), dummy),
                              Ext#'Extension'{extnValue=transform(Value,decode)}
		      end
	      end, Exts).

decode_otp_cert_polices(Ext, Value) ->
    %% RFC 3280 states that certificate users SHOULD gracefully handle
    %% explicitText with more than 200 characters.
    {ok, CPs} = 'OTP-PKIX':decode('OTPCertificatePolicies', Value),
    Ext#'Extension'{extnValue=[translate_cert_polices(CP) || CP <- CPs]}.

translate_cert_polices(#'OTPPolicyInformation'{policyIdentifier = Id, policyQualifiers = Qs0}) ->
    Qs = [translate_cert_polices(Q) || Q <- Qs0],
    #'PolicyInformation'{policyIdentifier = Id, policyQualifiers = Qs};
translate_cert_polices(#'OTPPolicyQualifierInfo'{policyQualifierId = Id, qualifier = Q0}) ->
    Q = case Q0 of
            #'OTPUserNotice'{noticeRef = Ref, explicitText = {Type, Text0}} ->
                Text = string:slice(Text0, 0, 350),
                #'UserNotice'{noticeRef = Ref, explicitText = {Type, Text}};
            Other ->
                Other
        end,
    #'PolicyQualifierInfo'{policyQualifierId = Id, qualifier = Q}.

encode_extensions(asn1_NOVALUE) ->
    asn1_NOVALUE;

encode_extensions(Exts) ->
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
		      case extension_id(Id) =/= undefined andalso
                          'PKIX1Implicit-2009':getenc_CertExtensions(Id)
                      of
			  false ->
                              Ext;
                          {_,_} ->
                              Ext;
			  EncodeExt when is_function(EncodeExt, 3) ->
                              %% Undocumented asn1 usage, but currently the only way
                              %% to decode extensions.
			      Value1 = pubkey_translation:encode(Value0),
                              Value = element(1,EncodeExt('ExtnType', Value1, dummy)),
			      Ext#'Extension'{extnValue= iolist_to_binary(Value)}
		      end
	      end, Exts).

encode_tbs(TBS=#'OTPTBSCertificate'{issuer=Issuer0,
				    subject=Subject0,
				    subjectPublicKeyInfo=Spki0,
				    extensions=Exts0}) ->
    Issuer  = transform(Issuer0,encode),
    Subject = transform(Subject0,encode),
    Spki = encode_supportedPublicKey(Spki0),
    Exts = encode_extensions(Exts0),
    TBS#'OTPTBSCertificate'{issuer=Issuer, subject=Subject,
			    subjectPublicKeyInfo=Spki,extensions=Exts}.

decode_tbs(TBS = #'OTPTBSCertificate'{issuer=Issuer0,
				      subject=Subject0,
				      subjectPublicKeyInfo=Spki0,
				      extensions=Exts0}) -> 
    Issuer  = transform(Issuer0,decode),
    Subject = transform(Subject0,decode),
    Spki = decode_supportedPublicKey(Spki0),
    Exts = decode_extensions(Exts0),
    TBS#'OTPTBSCertificate'{issuer=Issuer, subject=Subject,
			    subjectPublicKeyInfo=Spki,extensions=Exts}.

transform_sub_tree(asn1_NOVALUE,_) -> asn1_NOVALUE;
transform_sub_tree(TreeList,Func) ->
    [Tree#'GeneralSubtree'{base=transform(Name,Func)} || 
	Tree = #'GeneralSubtree'{base=Name} <- TreeList].
