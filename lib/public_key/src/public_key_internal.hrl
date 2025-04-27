%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-ifndef(public_key_internal).
-define(public_key_internal, true).

-include("AlgorithmInformation-2009.hrl").
-include("DSS.hrl").
-include("ECPrivateKey.hrl").

-include("OCSP-2024-08.hrl").
-undef('id-kp-OCSPSigning').

-include("OTP-PKIX.hrl").
-include("PKCS-1.hrl").

%%  Bug in ASN.1 compiler  (hardcode the correct value)
-undef('rSASSA-PSS-Default-Identifier').
-define('rSASSA-PSS-Default-Identifier',
        {'RSASSA-AlgorithmIdentifier',{1,2,840,113549,1,1,10},
         {'RSASSA-PSS-params',{'HashAlgorithm',{1,3,14,3,2,26},'NULL'},
          {'MaskGenAlgorithm',{1,2,840,113549,1,1,8},
           {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}},20,1}}).


-include("PKCS-3.hrl").
-include("PKIX-CommonTypes-2009.hrl").
-include("PKIX1Explicit-2009.hrl").

-include("PKIX1Implicit-2009.hrl").

-undef('id-md2').
-undef('id-md5').
-undef('id-sha1').
-undef('rsaEncryption').
-undef('md2WithRSAEncryption').
-undef('md5WithRSAEncryption').
-undef('sha1WithRSAEncryption').
-include("PKIXAlgs-2009.hrl").

-include("Safecurves-pkix-18.hrl").

-include("pubkey_defs.hrl").

-define(unspecified, 0).
-define(keyCompromise, 1).
-define(cACompromise, 2).
-define(affiliationChanged, 3).
-define(superseded, 4).
-define(cessationOfOperation, 5).
-define(certificateHold, 6).
-define(removeFromCRL, 8).
-define(privilegeWithdrawn, 9).
-define(aACompromise, 10).

-define('anyPolicy', {2,5,29,32,0}).

-record('ECParameters',
        {
         version,
         fieldID,
         curve,
         base,
         order,
         cofactor = asn1_NOVALUE
        }).

-record('Curve',
        {
         a,
         b,
         seed = asn1_NOVALUE
        }).

-record('FieldID',
        {
         fieldType,
         parameters
        }).

-record('Dss-Parms',
        {
         p,         % pos_integer()
         q,         % pos_integer()
         g          % pos_integer()
        }).

-record('SignatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PublicKeyAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

%% Superseded by SingleAttribute.
-record('AttributeTypeAndValue',
        {
         type,
         value
        }).

-endif. % -ifdef(public_key_internal).
