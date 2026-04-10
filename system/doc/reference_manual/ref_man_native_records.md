<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Native Records

A native record is a data structure for storing a fixed number of
elements in named fields. Unlike traditional tuple-based records
described in the [previous section](ref_man_records.md), a native
record is a distinct type.

> #### Change {: .info }
>
> Native records was introduced in Erlang/OTP 29.

> #### Warning {: .warning }
>
> Native records are considered experimental in Erlang/OTP 29;
> that is, if necessary, there can be incompatible changes in
> Erlang/OTP 30.

## Key Differences from Tuple Records

* Native records are not tuples
* No element/2 access
* No implicit undefined defaults
* Field names are available at runtime
* Definitions are module-scoped and versioned
* More robust under code upgrade and distribution

## Summary

Native records provide:

* Stronger guarantees than tuple-based records
* Better encapsulation via module scoping and export control
* Safer initialization (no implicit defaults)
* Runtime reflection support
* Predictable semantics across code versions

## Defining Native Records

A native record definition consists of the record name, optional type
parameters, and a set of named fields. Field names must be atoms. Each
field can have an optional default value and type annotation.

```text
-record #Name{Field1 [= Expr1],
              ...
              FieldN [= ExprN]}.
```

A default value must be a literal value or simple expression that can
be evaluated at compile time. The expression must not contain variables,
function calls, or record constrution.

A native record definition can be placed anywhere among the attributes
and function declarations of a module, but the definition must come
before any usage of the record.

By default, a native record is only accessible within the defining
module. To make it accessible to other modules, it must be exported.

A native record should never be defined in a header file.

## Exporting Records

```text
-export_record([Name/Arity, ...]).
```

## Importing Records

To use a native record defined in another module without fully
qualifying its name, use `-import_record`:

```text
-import_record(Module, [Name1, Name2, ..., NameN]).
```

`Module`, an atom, specifies which module to import records from
`Name1`, `Name2`, and so on are record names (atoms) that are to
be imported.

## Local Record Operations

```erlang
-record #pair{a, b}.

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

## External Record Operations

```erlang
make_pair(A, B) ->
    #pair_library:pair{a=A, b=B}.
```

```erlang
-import(pair_library, [pair]).

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

```erlang
-module(pair_library).
-record #pair{a, b}.
-export_record([pair]).
```

## Creating Records

The following expression creates a new `Name` record where the value
of each field `FieldI` is the value of evaluating the corresponding
expression `ExprI`:

```text
#Name{Field1=Expr1, ..., FieldK=ExprK}
#Module:Name{Field1=Expr1, ..., FieldK=ExprK}
```

The fields can be in any order, not necessarily the same order as in
the record definition, and fields can be omitted. Omitted fields are
assigned their respective default values.

It is an error if not all fields are given values either explicitly
or through default values. How the error is manifested depends on
whether the record construction is *local* or *external*.

### Local Record Construction

The operation is *local* if the first version of the syntax is used
(without module name) **and** there is a record definition for `Name`
in the module prior to the creation expression.

_Examples:_

```erlang
-module(example).
-export([make_pair/2]).

-record #pair{a, b}.

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

```erlang
1> example:make_pair(1, 2).
#example:pair{a = 1,b = 2}
```

Not giving values to all fields (explicitly or through default values)
in a records is always an error. For local record construction, it is
an compilation error.

_Examples:_

```erlang
-module(example).
-export([make_empty_pair/0]).

-record #pair{a, b}.

make_empty_pair() ->
    #pair{}.          % Compilation error.
```

### External Record Construction

The record construction is *external* if either of the following are true:

* The `#Module:Name{...}` syntax is used.
* The `#Name{...}` syntax is used **and** `Name` has been imported.

```erlang
-module(pair_library).
-record #pair{a, b}.
-export_record([pair]).
```

```erlang
-module(example).
-export([make_pair/2]).

make_pair(A, B) ->
    #pair_library:pair{a=A, b=B}.
```

```erlang
-module(example).
-export([make_pair/2]).

-import(pair_library, [pair]).

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

```erlang
-module(example).
-export([make_empty_pair/0]).

make_empty_pair() ->
    #pair_library:pair{}.
```

This module can be successfully compiled, but the record construction
will fail at runtime:

```erlang
1> example:make_empty_pair().
** novalue exception
```

## Record Field Access

```text
Expr#Name.Field
Expr#Module:Name.Field
```

Returns the value of the specified field. `Expr` is to evaluate to a `Name`
record.

_Example_:

```erlang
-record #person{name, phone, address}.

get_person_name(Person) ->
    Person#person.name.
```

## Anonymous Record Field Access

```text
Expr#_.Field
```

Returns the value of the specified field. `Expr` is to evaluate to a `Name`
record.

## Updating Native Records

```text
Expr#Name{Field1=Expr1, ..., FieldK=ExprK}
Expr#Module:Name{Field1=Expr1, ..., FieldK=ExprK}
```

`Expr` is to evaluate to a `Name` record. A copy of this record is
returned, with the value of each specified field `FieldI` changed to
the value of evaluating the corresponding expression `ExprI`. All
other fields retain their old values.

## Anonymous Update

```text
Expr#_{Field1=Expr1, ..., FieldK=ExprK}
```

`Expr` is to evaluate to a `Name` record. A copy of this record is
returned, with the value of each specified field `FieldI` changed to
the value of evaluating the corresponding expression `ExprI`. All
other fields retain their old values.

## Guard Tests

```text
is_record(Term) -> boolean().
is_record(Term, Name) -> boolean().
is_record(Term, Module, Name) -> boolean().
```

* is_record/1 checks if a term is any native record
* is_record/2 works with local or imported records
* is_record/3 checks module-qualified records

## Native Records in Guards

Accessing record fields is allowed in guards.

_Examples:_

```erlang
handle(Msg, State) when State#state.running =:= true ->
    ...
```

There is also a type test BIF [`is_record(Term, RecordTag)`](`is_record/2`).

_Example:_

```erlang
is_person(P) when is_record(P, person) ->
    true;
is_person(_P) ->
    false.
```

## Records in Patterns

Pattern matching uses the same syntax as creation:

```text
#Name{Field1=Expr1, ..., FieldN=ExprN}
#Module:Name{Field1=Expr1, ..., FieldN=ExprN}
```

In this case, one or more of `Expr1` ... `ExprN` can contain unbound
variables.

## Anonymous Pattern Matching

```text
#_{Field1=Pattern1, ..., FieldN=PatternN}
```

Matches any record containing the specified fields.

## Nested Records

Assume the following record definitions:

```erlang
-record #nrec0{name = "nested0"}.
-record #nrec1{name = "nested1", nrec0=#nrec0{}}.
-record #nrec2{name = "nested2", nrec1=#nrec1{}}.

N2 = #nrec2{},
```

Accessing or updating nested records can be written without parentheses:

```text
"nested0" = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
    N0n = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = "nested0a"},
```

which is equivalent to:

```text
"nested0" = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name,
N0n = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0{name = "nested0a"},
```
