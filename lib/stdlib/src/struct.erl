-module(struct).

-export([define/3, create/2, update/3]).

-spec define(module(), atom(), [{atom(), term()}]) -> term().
define(_Module, _Name, _KeyDefaultPairs) ->
  erlang:nif_error().

-spec create(module(), atom()) -> term().
create(_Module, _Name) ->
  erlang:nif_error().

-spec update(term(), atom(), term()) -> term().
update(_Object, _Key, _Value) ->
  erlang:nif_error().
