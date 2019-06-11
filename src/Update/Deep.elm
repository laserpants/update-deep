module Update.Deep exposing
    ( In, Update
    , addCmd, andMap, andThen, andThenIf, ap, applicationInit, documentInit, fold, foldAndThen, inState, applyCallback, join, kleisli, map, map2, map3, map4, map5, map6, map7, mapCmd, runUpdate, save, unwrap
    , andAddCmd, andApplyCallback
    )

{-|

TODO

# Update 

@docs Update, save, addCmd, map, mapCmd, applyCallback, fold, join, kleisli


## Chaining Updates

@docs andThen, andThenIf


## Applicative Interface

@docs andMap, ap, map2, map3, map4, map5, map6, map7


# Nested State

@docs In, inState


# Program Integration

@docs runUpdate, applicationInit, documentInit


# Helpers

@docs andAddCmd, andApplyCallback, foldAndThen, unwrap

-}


{-| A type alias wrapper for Elm's `( model, Cmd msg )` tuple which introduces a
third component to allow for one or more *callbacks* to be passed down in the update hierarchy.
-}
type alias Update m c e =
    ( m, Cmd c, List e )


{-| This function lifts a value into the `Update` context. For example, 

```
save model
```

corresponds to `( model, Cmd.none )` in code that doesn't use this library. Note that the `Update` 
type alias introduces a third element to this tuple, so if you want to construct a return value 
without relying on any of the library utilities, you'd have to write `( model, Cmd.none, [] )`.
-}
save : m -> Update m c e
save model =
    ( model, Cmd.none, [] )


{-| Add a command to the `Update` pipeline. For example;

    update msg state =
        case msg of
            SomeMsg someMsg ->
                state
                    |> addCmd someCommand
                    |> andThen (addCmd someOtherCommand)
                    |> andThen (setStatus Done)

In this example, `andThen (addCmd someOtherCommand)` can also be written as
[`andAddCmd`](#andAddCmd)` someOtherCommand`.

-}
addCmd : Cmd c -> m -> Update m c e
addCmd cmd state =
    ( state, cmd, [] )


{-| Apply a function to the command part of the value. This can be used to lift a
value returned from a nested update into the caller context. For example;

-}
mapCmd : (c -> d) -> Update m c e -> Update m d e
mapCmd f ( model, cmd, events ) =
    ( model, Cmd.map f cmd, events )


{-| Add a partially applied callback to the list of functions that is applied to 
the `Update` value returned from a nested call.
-}
applyCallback : e -> m -> Update m c e
applyCallback handler state =
    ( state, Cmd.none, [ handler ] )


{-| TODO
-}
ap : Update (a -> b) c e -> Update a c e -> Update b c e
ap ( f, cmda, e ) ( model, cmdb, e2 ) =
    ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )


{-| TODO
-}
andMap : Update a c e -> Update (a -> b) c e -> Update b c e
andMap a b =
    ap b a


{-| Apply a function to the state portion of a value.
-}
map : (a -> b) -> Update a c e -> Update b c e
map f ( model, cmd, events ) =
    ( f model, cmd, events )


{-| TODO
-}
map2 : (a -> b -> p) -> Update a c e -> Update b c e -> Update p c e
map2 f =
    ap << map f


{-| TODO
-}
map3 : (a -> b -> p -> q) -> Update a c e -> Update b c e -> Update p c e -> Update q c e
map3 f x =
    ap << map2 f x


{-| TODO
-}
map4 : (a -> b -> p -> q -> r) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e
map4 f x y =
    ap << map3 f x y


{-| TODO
-}
map5 : (a -> b -> p -> q -> r -> s) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e
map5 f x y z =
    ap << map4 f x y z


{-| TODO
-}
map6 : (a -> b -> p -> q -> r -> s -> t) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e
map6 f x y z a =
    ap << map5 f x y z a


{-| TODO
-}
map7 : (a -> b -> p -> q -> r -> s -> t -> u) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e -> Update u c e
map7 f x y z a b =
    ap << map6 f x y z a b


{-| Removes one level of monadic structure. Some other functions in this library are implemented in terms of `join`. In particular, `andThen f = join << map f`
-}
join : Update (Update a c e) c e -> Update a c e
join ( ( model, cmda, e ), cmdb, e2 ) =
    ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )


{-| Sequential composition of updates. This function is especially useful in combination
with the forward pipe operator (`|>`), for writing code in the style of pipelines. (`andThen` is
like the bind operator in Haskell, but with the arguments interchanged.)
-}
andThen : (b -> Update a c e) -> Update b c e -> Update a c e
andThen fun =
    join << map fun


{-| Like [`andThen`](#andThen) but only runs the function if the predicate, given as 
the first argument, evaluates to true.
-}
andThenIf : (a -> Bool) -> (a -> Update a c e) -> Update a c e -> Update a c e
andThenIf pred fun upd =
    map pred upd
        |> andThen
            (\cond ->
                if cond then
                    join (map fun upd)

                else
                    upd
            )


{-| Right-to-left (Kleisli) composition of two functions that return `Update` values,
passing the state part of the first return value to the second function.
-}
kleisli : (b -> Update d c e) -> (a -> Update b c e) -> (a -> Update d c e)
kleisli f g =
    andThen f << g


{-| Shortcut for `andThen << addCmd`
-}
andAddCmd : Cmd c -> Update a c e -> Update a c e
andAddCmd =
    andThen << addCmd


{-| Shortcut for `andThen << applyCallback`
-}
andApplyCallback : e -> Update a c e -> Update a c e
andApplyCallback =
    andThen << applyCallback


{-| Collapse the list of monadic functions (callbacks) produced by a nested update
call, sequentially composing one after another.
-}
fold : Update a c (a -> Update a c e) -> Update a c e
fold ( m, cmd, events ) =
    List.foldr andThen ( m, cmd, [] ) events


{-| Shortcut for `\f -> fold << andThen f`
-}

foldAndThen : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
foldAndThen fun =
    fold << andThen fun


{-| TODO rename?
-}
unwrap : (a -> b) -> (b -> a -> c) -> a -> c
unwrap get some state =
    some (get state) state


{-| TODO
-}
type alias In state slice msg a =
    (slice -> Update slice msg (state -> Update state msg a)) -> state -> Update state msg a


{-| TODO
-}
inState : { get : b -> d, set : b -> m -> a } -> (d -> Update m c (a -> Update a c e)) -> b -> Update a c e
inState { get, set } fun state =
    get state |> fun |> foldAndThen (set state >> save)


{-| Normally you shouldn't need to use this function directly in client code.
Instead, see the [`Update.Deep.Browser`](Update-Deep-Browser) module.
-}
applicationInit : (d -> e -> f -> Update a b c) -> d -> e -> f -> ( a, Cmd b )
applicationInit f a b c =
    let
        ( model, cmd, _ ) =
            f a b c
    in
    ( model, cmd )


{-| Normally you shouldn't need to use this function directly in client code.
Instead, see the [`Update.Deep.Browser`](Update-Deep-Browser) module.
-}
documentInit : (f -> Update a b c) -> f -> ( a, Cmd b )
documentInit f a =
    let
        ( model, cmd, _ ) =
            f a
    in
    ( model, cmd )


{-| Turn an update into a plain `( model, cmd )` pair, disregarding any callbacks.
-}
runUpdate : (d -> e -> Update a b c) -> d -> e -> ( a, Cmd b )
runUpdate f a b =
    let
        ( model, cmd, _ ) =
            f a b
    in
    ( model, cmd )
