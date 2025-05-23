%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Record and constant definitions for the TLS-handshake protocol
%% see RFC 5246. 
%%----------------------------------------------------------------------
-ifndef(tls_handshake).
-define(tls_handshake, true).

-include("ssl_handshake.hrl"). %% Common TLS and DTLS records and Constantes

-record(client_hello, {
	  client_version,
	  random,             
	  session_id,          % opaque SessionID<0..32>
	  cookie,              % opaque<2..2^16-1>
	  cipher_suites,       % cipher_suites<2..2^16-1>
	  %% Extensions
	  extensions
	 }).

-endif. % -ifdef(tls_handshake).
