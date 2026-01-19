%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(records).

%% BIFs (implemented in the runtime system).
-export([get/2, get_module/1, get_name/1, get_field_names/1,
         is_exported/1, create/3, create/4, update/4]).

-doc """
Returns the element having key `Key` in record `Record`.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec get(Key, Record) -> dynamic() when
      Key :: atom(),
      Record :: record().
get(_Key, _Record) ->
    erlang:nif_error(undefined).

-doc "Returns the module name for record `Record`.".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec get_module(Record) -> Module when
      Record :: record(),
      Module :: module().
get_module(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns the name of record `Record`.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec get_name(Record) -> Name when
      Record :: record,
      Name :: atom().
get_name(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns the field names (keys) for record `Record`.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec get_field_names(Record) -> [Name] when
      Record :: record(),
      Name :: atom().
get_field_names(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns `true` if record `Record` is exported; otherwise, returns false.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec is_exported(term()) -> boolean().
is_exported(_Record) ->
    erlang:nif_error(undefined).

-doc """
Creates a record.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec create(Module :: module(), RecordName :: atom(),
             FieldsMap :: #{atom() => term()}) -> record().
create(Module, RecordName, FieldsMap) ->
    create(Module, RecordName, FieldsMap, #{exported => false}).

-doc """
Creates a record.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec create(Module :: module(), RecordName :: atom(),
             FieldsMap :: #{atom() => term()},
             Options :: #{ exported => boolean() }) -> record().
create(_Module, _RecordName, _FieldsMap, _Options) ->
    erlang:nif_error(undefined).

-doc """
Updates a record.
""".
-doc #{ since => ~"OTP @OTP-19785@" }.
-spec update(Src :: record(), Module :: module(), RecordName :: atom(),
             FieldsMap :: #{atom() => term()}) -> record().
update(_Src, _Module, _RecordName, _FieldsMap) ->
    erlang:nif_error(undefined).
