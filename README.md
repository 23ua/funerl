# funerl

### Maybe monad

```erlang
1> Val = maybe:just(1).
{ok,1}
2> NoVal = maybe:nothing().
undefined
3> maybe:map(fun(X) -> X+1 end, Val).
{ok,2}
4> maybe:map(fun(X) -> X+1 end, NoVal).
undefined
```


### Either monad
```erlang
1> Result = either:right(23).
{ok,23}
2> Error = either:left(badarg).
{error,badarg}
3> either:map(fun(X) -> X * 2 end, Result).
{ok,46}
4> either:map(fun(X) -> X * 2 end, Error).
{error,badarg}
```

#### You can use these monads to handle errors:
```erlang
1> Result = either:right(23).
{ok,23}
2> Error = either:left(badarg).
{error,badarg}
3> either:map(fun(X) -> X * 2 end, Result).
{ok,46}
4> either:map(fun(X) -> X * 2 end, Error).
{error,badarg}
```

#### Using ```either:map/2``` and ```either:bind/2``` to prevent nested case expressions:
```erlang
-spec request_order() -> either:either(term()).
request_order(Order) ->
    ...
    either:right(NewOrder).
    
-spec validate_order(Order :: term()) -> either:either(term()).
validate_order(Order) ->
    either:left(order_invalid).

-spec db_write(atom(), Order :: term()) -> term().
db_write(Table, Value) when is_atom(Table) ->
    ...
    Result = db:insert(Table, Value),
    ...
    Result;


-spec add_order(term()) -> either:either().
add_order(Order) ->
    Res = request_order(Order),
    MaybeNewOrder = either:bind(fun validate_order/1, Res),

    WriteFun = fun(Ord) ->
        db_write(order, Ord)
    end,
    either:map(WriteFun, MaybeNewOrder).

```
Returns ```{error, order_invalid}```:
```erlang
1> Order = ...
2> Result = add_order(Order).
{error, order_invalid}
3> either:is_left(Result).
true
4> either:is_right(Result).
false
```
