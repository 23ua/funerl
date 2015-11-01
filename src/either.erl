%%%-------------------------------------------------------------------
%%% @author 23ua
%%% @copyright (C) 2015
%%% @doc
%%% Either monad implementation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(either).
-author("23ua").

%% API
-export([
    either/3,
    is_left/1,
    is_right/1,
    lefts/1,
    rights/1,
    left/1,
    right/1,
    map/2,
    partition/1,
    bind/2]).


-type either() :: left() | right().
-type left()   :: {error, any()}.
-type right()  :: {ok, any()}.


-spec either(fun(), fun(), either()) -> any().
either(LeftFun, RightFun, Either) ->
    case Either of
        {ok, Value} ->
            RightFun(Value);
        {error, _} ->
            LeftFun(Either)
    end.


-spec bind(either(), fun((term()) -> either())) -> any().
bind({ok, Val}, MonadFun) ->
    MonadFun(Val);

bind({error, _} = Left, _) ->
    Left.


-spec lefts([either()]) -> [either()].
lefts(Eithers) when is_list(Eithers) ->
    lists:filter(fun is_left/1, Eithers).


-spec rights([either()]) -> [either()].
rights(Eithers) when is_list(Eithers) ->
    lists:filter(fun is_right/1, Eithers).


-spec is_right(either()) -> boolean().
is_right({ok, _}) ->
    true;

is_right(_) ->
    false.


-spec is_left(either()) -> boolean().
is_left({error, _}) ->
    true;

is_left(_) ->
    false.


-spec map(fun(), either()) -> either().
map(Fun, {ok, Val}) ->
    right(Fun(Val));

map(_, {error, _} = Left) ->
    Left.


-spec right(any()) -> right().
right(Value) ->
    {ok, Value}.


-spec left(any()) -> left().
left(Reason) ->
    {error, Reason}.


-spec partition([either()]) -> {[left()], [right()]}.
partition([Eithers]) ->
    lists:partition(fun is_left/1, Eithers).

