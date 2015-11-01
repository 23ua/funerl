%%%-------------------------------------------------------------------
%%% @author 23ua
%%% @copyright (C) 2015
%%% @doc
%%% Maybe monad implementation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(maybe).
-author("23ua").

%% API
-export([
    map/2,
    just/1,
    nothing/0,
    is_just/1,
    is_nothing/1,
    from_just/1,
    from_maybe/2,
    cat_maybes/1,
    map_maybe/2,
    maybe_to_list/1,
    list_to_maybe/1,
    maybe/3,
    bind/2]).


-type maybe()   :: just() | nothing().
-type maybe(A)  :: just(A) | nothing().
-type just()    :: {ok, any()}.
-type just(A)   :: {ok, A}.
-type nothing() :: undefined.


-spec map(fun(), maybe()) -> maybe().
map(Fun, {ok, Val}) ->
    just(Fun(Val));

map(_, undefined) ->
    undefined.


-spec bind(maybe(), fun((term()) -> maybe())) -> any().
bind({ok, Val}, MonadFun) ->
    MonadFun(Val);

bind(undefined, _) ->
    nothing().


-spec maybe(Default :: any(), fun((any()) -> any()), maybe()) -> any().
maybe(_, Fun, {ok, Val}) ->
    Fun(Val);

maybe(Default, _, undefined) ->
    Default.


-spec just(any()) -> just().
just(Val) ->
    {ok, Val}.


-spec nothing() -> nothing().
nothing() ->
    undefined.


-spec is_just(maybe()) -> boolean().
is_just({ok, _}) ->
    true;

is_just(_) ->
    false.


-spec is_nothing(maybe()) -> boolean().
is_nothing(undefined) ->
    true;

is_nothing(_) ->
    false.


-spec from_just(just()) -> any().
from_just({ok, Val}) ->
    Val.


-spec from_maybe(A :: any(), maybe(A)) -> A.
from_maybe(_, {ok, Val}) ->
    Val;

from_maybe(Default, undefined) ->
    Default.


-spec cat_maybes([maybe(A)]) -> [A].
cat_maybes(Maybes) when is_list(Maybes) ->
    lists:filtermap(fun ({ok, Val}) -> {true, Val}; (_) -> false end, Maybes).


-spec map_maybe(maybe(), [any()]) -> [any()].
map_maybe(MapFun, ListIn) ->
    % FIXME: Maybe implement as filtermap for efficiency reasons
    MaybeVals = lists:map(MapFun, ListIn),
    cat_maybes(MaybeVals).


-spec maybe_to_list(maybe()) -> list().
maybe_to_list({ok, Val}) ->
    [Val];

maybe_to_list(_) ->
    [].


-spec list_to_maybe(list()) -> maybe().
list_to_maybe([]) ->
    nothing();

list_to_maybe([Val|_]) ->
    just(Val).

