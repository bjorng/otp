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
-moduledoc """
Native records processing functions.

This module contains functions for native records processing. More functions
may be added in a later release.
""".
-moduledoc(#{since => ~"OTP @OTP-19785@"}).

%% BIFs (implemented in the runtime system).
-export([get/2, get_module/1, get_name/1, get_field_names/1,
         is_exported/1, create/4, update/4]).

-doc """
Options that can be used when creating a native record.

Consumed by:

- [`records:create/4`](`create/4`)
""".
-type create_options() :: #{is_exported := boolean(),
                            order := [atom()]}.

-doc """
Returns value `Value` associated with `Key` if native record `Record`
contains `Key`.

This call fails with a `badarg` exception if `Record` is not a native record,
or if `Key` does not exist in `Record`.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:get(x, R).
1
3> records:get(y, R).
** exception error: bad argument
     in function  records:get/2
        called as records:get(y,#test:a{x = 1})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec get(Key, Record) -> dynamic() when
      Key :: atom(),
      Record :: record().
get(_Key, _Record) ->
    erlang:nif_error(undefined).

-doc """
Returns the module `Module` where the native record `Record` is defined in.

This call fails with a `badarg` exception if `Record` is not a native record.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:get_module(R).
test
3> records:get_module(#{}).
** exception error: bad argument
     in function  records:get_module/1
        called as records:get_module(#{})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec get_module(Record) -> Module when
      Record :: record(),
      Module :: module().
get_module(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns the name `Name` of the native record `Record`.

This call fails with a `badarg` exception if `Record` is not a native record.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:get_name(R).
a
3> records:get_name(#{}).
** exception error: bad argument
     in function  records:get_name/1
        called as records:get_name(#{})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec get_name(Record) -> Name when
      Record :: record,
      Name :: atom().
get_name(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns a complete list of field names (keys) in native record `Record`, in
the order of declaration.

This call fails with a `{badrecord,Record}` exception if `Record` is not a
native record.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:get_field_names(R).
[x]
3> records:get_field_names({x,y}).
** exception error: {badrecord,{x,y}}
     in function  records:get_field_names/1
        called as records:get_field_names({x,y})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec get_field_names(Record) -> [Name] when
      Record :: record(),
      Name :: atom().
get_field_names(_Record) ->
    erlang:nif_error(undefined).

-doc """
Returns `true` if native record `Record` is exported; otherwise, returns
`false`.

This call fails with a `{badrecord,Record}` exception if `Record` is not a
native record.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:is_exported(R).
false
3> records:is_exported({x,y}).
** exception error: {badrecord,{x,y}}
     in function  records:is_exported/1
        called as records:is_exported({x,y})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec is_exported(term()) -> boolean().
is_exported(_Record) ->
    erlang:nif_error(undefined).

-doc """
Takes a map `FieldsMap` and creates a native record `Record` with module
`Module` and name `RecordName`. The native record definition does
not have to exist in the given module.

The call fails with a `{badrecord,Record}` exception if `Module` or `RecordName`
are not atoms.
The call fails with a `{badmap, FieldsMap}` if `FieldsMap` is not a map.
The call fails with a `{novalue, Field}` if `Field` in `Record` neither has a
value by default or in `FieldsMap`.
The call fails with a `{badfield, Field}` if `Field` in `FieldsMap` does not
exist in `Record`.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> records:create(test,b,#{y=>1},#{}).
** exception error: bad argument
     in function  records:create/4
        called as records:create(test,b,#{y => 1},#{})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec create(Module :: module(), RecordName :: atom(),
             FieldsMap :: #{atom() => term()},
             Options :: create_options()) -> record().
create(_Module, _RecordName, _FieldsMap, _Options) ->
    erlang:nif_error(undefined).

-doc """
Takes a map `FieldsMap` and updates the native record `Src` as defined in
module `Module` with name `RecordName`.

The call fails with a `{badrecord,Record}` exception if `Src` is not a native
record defined in module `Module` with name `RecordName`.
The call fails with a `{badmap, FieldsMap}` if `FieldsMap` is not a map.
The call fails with a `{badfield, Field}` if `Field` in `FieldsMap` does not
exist in `Record`.

## Examples

```erlang
1> R = records:create(test,a,#{x=>1},#{is_exported=>false,order=>[x]}).
#test:a{x = 1}
2> Updated = records:update(R, test, a, #{x=>2}).
#test:a{x = 2}
3> records:update(R, test, a, #{y=>1}).
** exception error: bad field name: y
     in function  records:update/4
        called as records:update(#test:a{x = 1},test,a,#{y => 1})
```
""".
-doc #{since => ~"OTP @OTP-19785@"}.
-spec update(Src :: record(), Module :: module(), RecordName :: atom(),
             FieldsMap :: #{atom() => term()}) -> record().
update(_Src, _Module, _RecordName, _FieldsMap) ->
    erlang:nif_error(undefined).
