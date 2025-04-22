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

-include("OCSP-2009.hrl").
-undef('id-kp-OCSPSigning').

-include("OTP-PKIX.hrl").
-include("PKCS-1.hrl").
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

-include("PKCS-FRAME.hrl").

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

-record('ECParameters',
        {
         version,    % pos_integer()
         fieldID,    % #'FieldID'{}
         curve,      % #'Curve'{}
         base,       % binary()
         order,      % pos_integer()
         cofactor    % pos_integer()
        }).

-record('Curve',
        {
          a,        % binary()
          b,        % binary()
          seed      % bitstring() - optional
        }).

-record('FieldID',
         {
           fieldType,    % oid()
           parameters    % Depending on fieldType
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

-endif. % -ifdef(public_key_internal).
