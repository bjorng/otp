-module(struct_prototype).
-compile([export_all, nowarn_export_all]).

%% f(),
%% struct_prototype:define(a,b,{{field1,1},{field2,2}}),
%% Obj = struct_prototype:create(a,b,2),
%% Field1Prior = struct_prototype:read(Obj, field1),
%% Obj2 = struct_prototype:update(Obj, field1, blurf),
%% Obj3 = struct_prototype:update(Obj2, field1, 1),
%% {Obj =:= Obj3, Obj2, Obj3}.

define(_Module, _Name, _KeyDefaultPairs) ->
    erlang:nif_error().

create(_Module, _Name, _Arity) ->
    erlang:nif_error().

update(_Object, _Key, _Value) ->
    erlang:nif_error().

read(_Object, _Key) ->
    erlang:nif_error().

is(_Object, _Module, _Name, _Arity) ->
    erlang:nif_error().
