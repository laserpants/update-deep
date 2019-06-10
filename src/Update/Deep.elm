module Update.Deep exposing
    ( In, Update
    , addCmd, andMap, andThen, andThenIf, ap, applicationInit, documentInit, foldEvents, foldEventsAndThen, inState, invokeHandler, join, kleisli, map, map2, map3, map4, map5, map6, map7, mapCmd, runUpdate, save, unwrap
    , andAddCmd, andInvokeHandler
    )

{-|


# Types

@docs In, Update


# Functions

@docs addCmd, andMap, andThen, andThenIf, ap, applicationInit, documentInit, foldEvents, foldEventsAndThen, inState, invokeHandler, join, kleisli, map, map2, map3, map4, map5, map6, map7, mapCmd, runUpdate, save, unwrap


# Shortcuts

@docs andAddCmd, andInvokeHandler

-}


{-| A type alias wrapper for Elm's `( model, Cmd msg )` tuple that introduces a
third component to allow for callbacks to be passed down in the update hierarchy.
-}
type alias Update m c e =
    ( m, Cmd c, List e )


{-| This function lifts a value into the `Update` context. E.g., `save someState` is the same as `( someState, Cmd.none )` in code that doesn't use this library.
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

-}
addCmd : Cmd c -> m -> Update m c e
addCmd cmd state =
    ( state, cmd, [] )


{-| Apply a function to the command part of the value. This is typically used to lift a
value returned from a nested update into the caller context.
-}
mapCmd : (c -> d) -> Update m c e -> Update m d e
mapCmd f ( model, cmd, events ) =
    ( model, Cmd.map f cmd, events )


{-| TODO
-}
invokeHandler : e -> m -> Update m c e
invokeHandler handler state =
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


{-| Apply a function to the state portion of the value.
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


{-| Removes one level of monadic structure. In particular, `andThen f = join << map f`
-}
join : Update (Update a c e) c e -> Update a c e
join ( ( model, cmda, e ), cmdb, e2 ) =
    ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )


{-| Sequential composition of updates. This function is especially useful in combination
with the forward pipe operator (|>), for writing code in the style of pipelines. It is
just like the bind operator in Haskell, but with the arguments interchanged.
-}
andThen : (b -> Update a c e) -> Update b c e -> Update a c e
andThen fun =
    join << map fun


{-| TODO
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


{-| Shortcut for `andThen << invokeHandler`
-}
andInvokeHandler : e -> Update a c e -> Update a c e
andInvokeHandler =
    andThen << invokeHandler


{-| Collapse the list of monadic functions (events) produced by a nested update
call, and merge them into the current context.
-}
foldEvents : Update a c (a -> Update a c e) -> Update a c e
foldEvents ( m, cmd, events ) =
    List.foldr andThen ( m, cmd, [] ) events


{-| Shortcut for `\f -> foldEvents << andThen f`
-}
foldEventsAndThen : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
foldEventsAndThen fun =
    foldEvents << andThen fun


{-| TODO
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
    get state |> fun |> foldEventsAndThen (set state >> save)


{-| TODO
-}
applicationInit : (d -> e -> f -> Update a b c) -> d -> e -> f -> ( a, Cmd b )
applicationInit f a b c =
    let
        ( model, cmd, _ ) =
            f a b c
    in
    ( model, cmd )


{-| TODO
-}
documentInit : (f -> Update a b c) -> f -> ( a, Cmd b )
documentInit f a =
    let
        ( model, cmd, _ ) =
            f a
    in
    ( model, cmd )


{-| TODO
-}
runUpdate : (d -> e -> Update a b c) -> d -> e -> ( a, Cmd b )
runUpdate f a b =
    let
        ( model, cmd, _ ) =
            f a b
    in
    ( model, cmd )
