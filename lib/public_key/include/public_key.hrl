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

-ifndef(public_key).
-define(public_key, true).

%%%
%%% RSA
%%%

-record('RSAPublicKey',
        {
         modulus,       % pos_integer()
         publicExponent % pos_integer()
        }).

-record('RSAPrivateKey',
        {
         version,         % two-prime | multi
         modulus,         % pos_integer()
         publicExponent,  % pos_integer()
         privateExponent, % pos_integer()
         prime1,          % pos_integer()
         prime2,          % pos_integer()
         exponent1,       % pos_integer()
         exponent2,       % pos_integer()
         coefficient,     % pos_integer()
         otherPrimeInfos  % [#OtherPrimeInfo{})] | asn1_NOVALUE
  }).

-record('OtherPrimeInfo',
        {
         prime,           % pos_integer()
         exponent,        % pos_integer()
         coefficient      % pos_integer()
        }).

-record('RSASSA-PSS-params',
        {
         hashAlgorithm,     % #'HashAlgorithm'{})},
         maskGenAlgorithm,  % #'MaskGenAlgorithm'{}},
         saltLength,        % pos_integer(),
         trailerField      % pos_integer()
        }).

-record('HashAlgorithm',
        {
         algorithm,  % oid()
         parameters  % defaults to asn1_NOVALUE
        }).

-record('MaskGenAlgorithm',
        {
         algorithm,  % oid()
         parameters  % defaults to asn1_NOVALUE
        }).

%%%
%%% DSA
%%%

-record('DSAPrivateKey',
        {
         version,      % pos_integer()
         p,            % pos_integer()
         q,            % pos_integer()
         g,            % pos_integer()
         y,            % pos_integer()
         x             % pos_integer()
        }).

-record('Dss-Parms',
        {
         p,         % pos_integer()
         q,         % pos_integer()
         g          % pos_integer()
        }).

%%%
%%% ECDSA and EDDSA
%%%

-define('id-Ed25519', {1,3,101,112}).
-define('id-Ed448', {1,3,101,113}).

%% Undocumented but used by test suite.
-define('sect571r1', {1,3,132,0,39}).
-define('sect571k1', {1,3,132,0,38}).
-define('sect409r1', {1,3,132,0,37}).
-define('sect409k1', {1,3,132,0,36}).
-define('secp521r1', {1,3,132,0,35}).
-define('secp384r1', {1,3,132,0,34}).
-define('secp224r1', {1,3,132,0,33}).
-define('secp224k1', {1,3,132,0,32}).
-define('secp192k1', {1,3,132,0,31}).
-define('secp160r2', {1,3,132,0,30}).
-define('secp128r2', {1,3,132,0,29}).
-define('secp128r1', {1,3,132,0,28}).
-define('sect233r1', {1,3,132,0,27}).
-define('sect233k1', {1,3,132,0,26}).
-define('sect193r2', {1,3,132,0,25}).
-define('sect193r1', {1,3,132,0,24}).
-define('sect131r2', {1,3,132,0,23}).
-define('sect131r1', {1,3,132,0,22}).
-define('sect283r1', {1,3,132,0,17}).
-define('sect283k1', {1,3,132,0,16}).
-define('sect163r2', {1,3,132,0,15}).
-define('secp256k1', {1,3,132,0,10}).
-define('secp160k1', {1,3,132,0,9}).
-define('secp160r1', {1,3,132,0,8}).
-define('secp112r2', {1,3,132,0,7}).
-define('secp112r1', {1,3,132,0,6}).
-define('sect113r2', {1,3,132,0,5}).
-define('sect113r1', {1,3,132,0,4}).
-define('sect239k1', {1,3,132,0,3}).
-define('sect163r1', {1,3,132,0,2}).
-define('sect163k1', {1,3,132,0,1}).
-define('secp256r1', {1,2,840,10045,3,1,7}).
-define('secp192r1', {1,2,840,10045,3,1,1}).

-record('ECPrivateKey',
        {
         version,       % pos_integer()
         privateKey,    % binary()
         parameters,    % {ecParameters, #'ECParameters'{}} |
                                                % {namedCurve, Oid::tuple()} |
                                                % {implicitlyCA, 'NULL'}
         publicKey,      % bitstring()
         attributes  %% Not documented
        }).

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
        }).        %


-record('FieldID',
        {
         fieldType,    % oid()
         parameters    % Depending on fieldType
        }).

-record('ECPoint',
        {
         point %  binary() - the public key
        }).

%%%
%%% PKIX Certificates
%%%

-record('Certificate',
        {
         tbsCertificate,        % #'TBSCertificate'{}
         signatureAlgorithm,    % #'AlgorithmIdentifier'{}
         signature              % bitstring()
        }).

-record('TBSCertificate',
        {
         version,              % v1 | v2 | v3
         serialNumber,         % pos_integer()
         signature,            % #'AlgorithmIdentifier'{}
         issuer,               % {rdnSequence, [#AttributeTypeAndValue'{}]
         validity,             % #'Validity'{}
         subject,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
         subjectPublicKeyInfo, % #'SubjectPublicKeyInfo'{}
         issuerUniqueID,       % binary() | asn1_novalue
         subjectUniqueID,      % binary() | asn1_novalue
         extensions            % [#'Extension'{}]
        }).

-record('AlgorithmIdentifier',
        {
         algorithm,  % oid()
         parameters  % der_encoded()
        }).


%%%
%%% Erlang alternate representation of PKIX certificate
%%%

-record('OTPCertificate',
        {
         tbsCertificate,        % #'OTPTBSCertificate'{}
         signatureAlgorithm,    % #'SignatureAlgorithm'
         signature              % bitstring()
        }).

-record('OTPTBSCertificate',
        {
         version,              % v1 | v2 | v3
         serialNumber,         % pos_integer()
         signature,            % #'SignatureAlgorithm'
         issuer,               % {rdnSequence, [#AttributeTypeAndValue'{}]}
         validity,             % #'Validity'{}
         subject,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
         subjectPublicKeyInfo, % #'OTPSubjectPublicKeyInfo'{}
         issuerUniqueID,       % binary() | asn1_novalue
         subjectUniqueID,      % binary() | asn1_novalue
         extensions            % [#'Extension'{}]
        }).

-record('SignatureAlgorithm',
        {
         algorithm,  % id_signature_algorithm()
         parameters  % asn1_novalue | #'Dss-Parms'{}
        }).

-define('id-dsa-with-sha1', {1,2,840,10040,4,3}).
-define('id-dsaWithSHA1', {1,3,14,3,2,27}).     %Probably obsolete.
-define('md2WithRSAEncryption', {1,2,840,113549,1,1,2}).
-define('md5WithRSAEncryption', {1,2,840,113549,1,1,4}).
-define('sha1WithRSAEncryption', {1,2,840,113549,1,1,5}).
-define('sha-1WithRSAEncryption', {1,3,14,3,2,29}). %Probably obsolete.
-define('sha224WithRSAEncryption', {1,2,840,113549,1,1,14}).
-define('sha256WithRSAEncryption', {1,2,840,113549,1,1,11}).
-define('sha512WithRSAEncryption', {1,2,840,113549,1,1,13}).
-define('ecdsa-with-SHA1', {1,2,840,10045,4,1}).

%% Undocumented but used by test suite.
-define('id-dsa-with-sha224', {2,16,840,1,101,3,4,3,1}).
-define('id-dsa-with-sha256', {2,16,840,1,101,3,4,3,2}).
-define('id-sha1', {1,3,14,3,2,26}).
-define('id-sha224', {2,16,840,1,101,3,4,2,4}).
-define('id-sha256', {2,16,840,1,101,3,4,2,1}).
-define('id-sha384', {2,16,840,1,101,3,4,2,2}).
-define('id-sha512', {2,16,840,1,101,3,4,2,3}).
-define('sha384WithRSAEncryption', {1,2,840,113549,1,1,12}).
-define('id-RSASSA-PSS', {1,2,840,113549,1,1,10}).
-define('ecdsa-with-SHA256', {1,2,840,10045,4,3,2}).
-define('ecdsa-with-SHA384', {1,2,840,10045,4,3,3}).
-define('ecdsa-with-SHA512', {1,2,840,10045,4,3,4}).
-define('rSASSA-PSS-Default-Identifier', {'RSASSA-AlgorithmIdentifier',{1,2,840,113549,1,1,10},{'RSASSA-PSS-params',{'HashAlgorithm',{1,3,14,3,2,26},'NULL'},{'MaskGenAlgorithm',{1,2,840,113549,1,1,8},{'HashAlgorithm',{1,3,14,3,2,26},'NULL'}},20,1}}).
-define('id-mgf1', {1,2,840,113549,1,1,8}).

-record('AttributeTypeAndValue',
        {
         type,   % id_attributes()
         value   % term()
        }).

-define('id-at-name', {2,5,4,41}).
-define('id-at-surname', {2,5,4,4}).
-define('id-at-givenName', {2,5,4,42}).
-define('id-at-initials', {2,5,4,43}).
-define('id-at-generationQualifier', {2,5,4,44}).
-define('id-at-commonName', {2,5,4,3}).
-define('id-at-localityName', {2,5,4,7}).
-define('id-at-stateOrProvinceName', {2,5,4,8}).
-define('id-at-organizationName', {2,5,4,10}).
-define('id-at-title', {2,5,4,12}).
-define('id-at-dnQualifier', {2,5,4,46}).
-define('id-at-countryName', {2,5,4,6}).
-define('id-at-serialNumber', {2,5,4,5}).
-define('id-at-pseudonym', {2,5,4,65}).

%% Not documented but used by the test suite.
-define('id-emailAddress', {1,2,840,113549,1,9,1}).
-define('id-at-organizationalUnitName', {2,5,4,11}).

%%%
%%% Validity, SubjectPublicKeyInfo, and SubjectPublicKeyInfoAlgorithm
%%%

-record('Validity',
        {
         notBefore, % time()
         notAfter   % time()
        }).

-record('SubjectPublicKeyInfo',
        {
         algorithm,       % #'PublicKeyAlgorithm'{}
         subjectPublicKey % binary()
        }).

-record('PublicKeyAlgorithm',
        {
         algorithm,  % id_public_key_algorithm()
         parameters  % public_key_params()
        }).


%%%
%%% Public-key algorithms
%%%
-define('rsaEncryption', {1,2,840,113549,1,1,1}).
-define('id-dsa', {1,2,840,10040,4,1}).
-define('dhpublicnumber', {1,2,840,10046,2,1}).
-define('id-keyExchangeAlgorithm', {2,16,840,1,101,2,1,1,22}).
-define('id-ecPublicKey', {1,2,840,10045,2,1}).

-record('Extension',
        {
         extnID,    % id_extensions() | oid()
         critical,  % boolean()
         extnValue  % der_encoded()
        }).

%%%
%%% Standard Certificate Extensions
%%%

-define('id-ce-targetInformation', {2,5,29,55}).
-define('id-ce-invalidityDate', {2,5,29,24}).
-define('id-ce-holdInstructionCode', {2,5,29,23}).
-define('id-ce-certificateIssuer', {2,5,29,29}).
-define('id-ce-cRLReasons', {2,5,29,21}).
-define('id-ce-deltaCRLIndicator', {2,5,29,27}).
-define('id-ce-issuingDistributionPoint', {2,5,29,28}).
-define('id-ce-cRLNumber', {2,5,29,20}).
-define('id-ce-freshestCRL', {2,5,29,46}).
-define('id-ce-inhibitAnyPolicy', {2,5,29,54}).
-define('id-ce-extKeyUsage', {2,5,29,37}).
-define('id-ce-cRLDistributionPoints', {2,5,29,31}).
-define('id-ce-policyConstraints', {2,5,29,36}).
-define('id-ce-nameConstraints', {2,5,29,30}).
-define('id-ce-basicConstraints', {2,5,29,19}).
-define('id-ce-subjectDirectoryAttributes', {2,5,29,9}).
-define('id-ce-issuerAltName', {2,5,29,18}).
-define('id-ce-subjectAltName', {2,5,29,17}).
-define('id-ce-policyMappings', {2,5,29,33}).
-define('id-ce-certificatePolicies', {2,5,29,32}).
-define('id-ce-privateKeyUsagePeriod', {2,5,29,16}).
-define('id-ce-keyUsage', {2,5,29,15}).
-define('id-ce-subjectKeyIdentifier', {2,5,29,14}).
-define('id-ce-authorityKeyIdentifier', {2,5,29,35}).

%% Not documented but used by test suite.
-define('anyExtendedKeyUsage', {2,5,29,37,0}).
-define('anyPolicy', {2,5,29,32,0}).


-record('AuthorityKeyIdentifier',
        {
         keyIdentifier,            % oid()
         authorityCertIssuer,      % general_name()
         authorityCertSerialNumber % pos_integer()
        }).

-record('PrivateKeyUsagePeriod',
        {
         notBefore,   % general_time()
         notAfter     % general_time()
        }).

-record('PolicyInformation',
        {
         policyIdentifier,  % oid()
         policyQualifiers   % [#PolicyQualifierInfo{}]
        }).

-record('PolicyQualifierInfo',
        {
         policyQualifierId,   % oid()
         qualifier            % string() | #'UserNotice'{}
        }).

-record('UserNotice',
        {
         noticeRef,   % #'NoticeReference'{}
         explicitText % string()
        }).

-record('NoticeReference',
        {
         organization,    % string()
         noticeNumbers    % [pos_integer()]
        }).

-record('PolicyMappings_SEQOF',
        {
         issuerDomainPolicy,  % oid()
         subjectDomainPolicy  % oid()
        }).

-record('Attribute',
        {
         type,  % oid()
         values % [der_encoded()]
        }).

-record('BasicConstraints',
        {
         cA,               % boolean()
         pathLenConstraint % pos_integer()
        }).

-record('NameConstraints',
        {
         permittedSubtrees, % [#'GeneralSubtree'{}]
         excludedSubtrees   % [#'GeneralSubtree'{}]
        }).

-record('GeneralSubtree',
        {
         base,    % general_name()
         minimum, % pos_integer()
         maximum  % pos_integer()
        }).

-record('PolicyConstraints',
        {
         requireExplicitPolicy, % pos_integer()
         inhibitPolicyMapping   % pos_integer()
        }).

-record('DistributionPoint',
        {
         distributionPoint,
         reasons,           % [dist_reason()]
         cRLIssuer          % [general_name()]
        }).

-record('AccessDescription',
        {
         accessMethod,    % oid()
         accessLocation   % general_name()
        }).

%%%
%%% CRL and CRL Extensions Profile
%%%

-record('CertificateList',
        {
         tbsCertList,        % #'TBSCertList{}
         signatureAlgorithm, % #'AlgorithmIdentifier'{}
         signature           % bitstring()
        }).

-record('TBSCertList',
        {
         version,             % v2 (if defined)
         signature,           % #AlgorithmIdentifier{}
         issuer,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
         thisUpdate,          % time()
         nextUpdate,          % time()
         revokedCertificates, % [#'TBSCertList_revokedCertificates_SEQOF'{}]
         crlExtensions        % [#'Extension'{}]
        }).

-record('TBSCertList_revokedCertificates_SEQOF',
        {
         userCertificate,      % pos_integer()
         revocationDate,       % timer()
         crlEntryExtensions    % [#'Extension'{}]
        }).

%%%
%%% CRL Extensions
%%%

-record('IssuingDistributionPoint',
        {
         distributionPoint,
         onlyContainsUserCerts,     % boolean()
         onlyContainsCACerts,       % boolean()
         onlySomeReasons,           % [dist_reason()]
         indirectCRL,               % boolean()
         onlyContainsAttributeCerts % boolean()
        }).

%%%
%%% PKCS#10 Certification Request
%%%

-record('CertificationRequest',
        {
   certificationRequestInfo, % #'CertificationRequestInfo'{},
   signatureAlgorithm,       % #'CertificationRequest_signatureAlgorithm'{}}.
   signature                 % bitstring()
  }).

-record('CertificationRequestInfo',
        {
         version,       % atom(),
         subject,       % {rdnSequence, [#AttributeTypeAndValue'{}]} ,
         subjectPKInfo, % #'CertificationRequestInfo_subjectPKInfo'{},
         attributes     % [#'AttributePKCS-10' {}]
        }).

-record('CertificationRequestInfo_subjectPKInfo',
        {
         algorithm,        % #'CertificationRequestInfo_subjectPKInfo_algorithm'{}
         subjectPublicKey  %  bitstring()
        }).

-record('CertificationRequestInfo_subjectPKInfo_algorithm',
        {
         algorithm,  % oid(),
         parameters  % der_encoded()
        }).

-record('CertificationRequest_signatureAlgorithm',
        {
         algorithm,  % oid(),
         parameters  % der_encoded()
        }).

-record('AttributePKCS-10',
        {
         type,   % oid(),
         values  % [der_encoded()]
        }).

-define(DEFAULT_VERIFYFUN,
	{fun(_,{bad_cert, _} = Reason, _) ->
		 {fail, Reason};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState};
	    (_, valid_peer, UserState) ->
		 {valid, UserState}
	 end, []}).

-record(path_validation_state,
        {
         valid_policy_tree,
         user_initial_policy_set,
         explicit_policy,
         inhibit_any_policy,
         inhibit_policy_mapping,
         policy_mapping_ext,
         policy_constraint_ext,
         policy_inhibitany_ext,
         policy_ext_present,
         policy_ext_any,
         current_any_policy_qualifiers,
         cert_num,
         last_cert = false,
         permitted_subtrees = no_constraints, %% Name constraints
         excluded_subtrees = [],      %% Name constraints
         working_public_key_algorithm,
         working_public_key,
         working_public_key_parameters,
         working_issuer_name,
         max_path_length,
         verify_fun,
         user_state
        }).

-record(revoke_state,
        {
         reasons_mask,
         cert_status,
         interim_reasons_mask,
         valid_ext,
         details
        }).

-record(cert,
        {
         der :: public_key:der_encoded(),
         otp :: #'OTPCertificate'{}
        }).

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

%%%
%%% OCSP, undocumented, but used by test suite.
%%%

-record('BasicOCSPResponse', {
  tbsResponseData,
  signatureAlgorithm,
  signature,
  certs = asn1_NOVALUE
}).

-record('SingleResponse', {
  certID,
  certStatus,
  thisUpdate,
  nextUpdate = asn1_NOVALUE,
  singleExtensions = asn1_NOVALUE
}).

-record('CertID', {
  hashAlgorithm,
  issuerNameHash,
  issuerKeyHash,
  serialNumber
}).

-record('ResponseData', {
  version = asn1_DEFAULT,
  responderID,
  producedAt,
  responses,
  responseExtensions = asn1_NOVALUE
}).

-define('id-kp-OCSPSigning', {1,3,6,1,5,5,7,3,9}).
-define('id-kp-timeStamping', {1,3,6,1,5,5,7,3,8}).
-define('id-kp-emailProtection', {1,3,6,1,5,5,7,3,4}).
-define('id-kp-codeSigning', {1,3,6,1,5,5,7,3,3}).
-define('id-kp-clientAuth', {1,3,6,1,5,5,7,3,2}).
-define('id-kp-serverAuth', {1,3,6,1,5,5,7,3,1}).

%%%
%%% OTP-PKIX.
%%%
%%% Undocumented but used but used in many places.
%%%

-record('OTPSubjectPublicKeyInfo', {
  algorithm,
  subjectPublicKey
}).

%%%
%%% Undocumented but used by test suite.
%%%

-record('DHParameter', {
  prime,
  base,
  privateValueLength = asn1_NOVALUE
}).

-record('RSASSA-AlgorithmIdentifier', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-define('id-pkix-ocsp-nonce', {1,3,6,1,5,5,7,48,1,2}).

%%%
%%% Undocumented but used by SSL.
%%%

-define('id-X25519', {1,3,101,110}).
-define('id-X448', {1,3,101,111}).
-define('brainpoolP512r1', {1,3,36,3,3,2,8,1,1,13}).
-define('brainpoolP384r1', {1,3,36,3,3,2,8,1,1,11}).
-define('brainpoolP256r1', {1,3,36,3,3,2,8,1,1,7}).

-record('PrivateKeyInfo', {   %% OneAsymmetricKey
  version,
  privateKeyAlgorithm,
  privateKey,
  attributes = asn1_NOVALUE,
  %% Version 2 with extension
  publicKey = asn1_NOVALUE
}).

-record('PrivateKeyInfo_privateKeyAlgorithm', {
  algorithm,
  parameters = asn1_NOVALUE
}).

%%%
%%% Undocumented but used by SSH.
%%%

-record('ECDSA-Sig-Value', {
  r,
  s
}).

-record('Dss-Sig-Value', {
  r,
  s
}).

-endif. % -ifdef(public_key).
