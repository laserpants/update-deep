module Update.Deep exposing
    ( Update, save, addCmd, map, mapCmd, applyCallback, fold, join, kleisli
    , andThen, andThenIf
    , andMap, ap, map2, map3, map4, map5, map6, map7
    , In, WrapIn, inState, wrapInState
    , runUpdate, applicationInit, documentInit
    , andAddCmd, andApplyCallback, foldAndThen, with
    )

{-| Utilities for updating nested state with the help of callbacks.


# Update

@docs Update, save, addCmd, map, mapCmd, applyCallback, fold, join, kleisli


## Chaining Updates

@docs andThen, andThenIf


## Applicative Interface

These functions address the need to map over functions of more than one argument.

@docs andMap, ap, map2, map3, map4, map5, map6, map7


# Nested State

@docs In, WrapIn, inState, wrapInState


# Program Integration

@docs runUpdate, applicationInit, documentInit


# Helpers

@docs andAddCmd, andApplyCallback, foldAndThen, with

-}


{-| A type alias wrapper for Elm's `( model, Cmd msg )` tuple which introduces a
third component to allow for one or more _callbacks_ to be passed down in the update hierarchy.
-}
type alias Update m c e =
    ( m, List (Cmd c), List e )


{-| This function lifts a value into the `Update` context. For example,

    save model

corresponds to `( model, Cmd.none )` in code that doesn't use this library. Note that the `Update`
type alias introduces a third element to this tuple, so if you want to construct a return value
without relying on any of the library utilities, you'd have to write `( model, Cmd.none, [] )`.

-}
save : m -> Update m c e
save model =
    ( model, [], [] )


{-| Add a command to an `Update` pipeline. For example;

    update msg state =
        case msg of
            SomeMsg someMsg ->
                state
                    |> addCmd someCommand
                    |> andThen (addCmd someOtherCommand)
                    |> andThen (setStatus Done)

In this example, `andThen (addCmd someOtherCommand)` can also be shortened to
[`andAddCmd`](#andAddCmd)`someOtherCommand`.

-}
addCmd : Cmd c -> m -> Update m c e
addCmd cmd state =
    ( state, [ cmd ], [] )


{-| Map over the Cmd contained in the provided `Update`. This can be used to lift a
value returned from a nested update into the caller context. For example;

    buttonUpdate : ButtonMsg -> ButtonState -> Update ButtonState ButtonMsg a
    buttonUpdate buttonMsg buttonState = ...

    type Msg
        = ButtonMsg ButtonMsg

    type alias State =
        { button : ButtonState }

    inButton : In State ButtonState msg a
    inButton =
        inState { get = .button, set = \state button -> { state | button = button } }

    update : Msg -> State -> Update ButtonState ButtonMsg a
    update msg state =
        case msg of
            ButtonMsg buttonMsg ->
                state
                    |> inButton (buttonUpdate buttonMsg)
                    |> mapCmd ButtonMsg

-}
mapCmd : (c -> d) -> Update m c e -> Update m d e
mapCmd f ( model, cmds, events ) =
    ( model, List.map (Cmd.map f) cmds, events )


{-| In a nested update call, append a partially applied callback to the list of
functions subsequently applied to the returned value.

Refer to the [examples](https://github.com/laserpants/elm-update-deep/tree/master/examples) and the [README](https://github.com/laserpants/elm-update-deep/blob/master/README.md) file for more on how to use this.

-}
applyCallback : e -> m -> Update m c e
applyCallback handler state =
    ( state, [], [ handler ] )


{-| See [`andMap`](#andMap). This function is the same but with the arguments interchanged.
-}
ap : Update (a -> b) c e -> Update a c e -> Update b c e
ap ( f, cmds1, e ) ( model, cmds2, e2 ) =
    ( f model, cmds1 ++ cmds2, e ++ e2 )


{-| Trying to map over a function `number -> number -> number`,

    map (+) (save 4)

we end up with a result of type `Update (number -> number) c e`. To apply the function inside this value to another `Update number c e` value, we can write&hellip;

    map (+) (save 4) |> andMap (save 5)

in `elm repl`, we can verify that the result is what we expect:

    > (map (+) (save 4) |> andMap (save 5)) == save 9
    True : Bool

This pattern scales in a nice way to functions of any number of arguments:

    let
        f x y z =
            x + y + z
    in
    map f (save 1)
        |> andMap (save 1)
        |> andMap (save 1)

If not sooner, you'll need this when you want to `mapN` and N > 7.

See also [`map2`](#map2), [`map3`](#map3), etc.

-}
andMap : Update a c e -> Update (a -> b) c e -> Update b c e
andMap a b =
    ap b a


{-| Apply a function to the state portion of a value.
-}
map : (a -> b) -> Update a c e -> Update b c e
map f ( model, cmd, events ) =
    ( f model, cmd, events )


{-| Apply a function of two arguments to the state portion of a value.
Equivalently, we can think of this as taking a function `a -> b -> c` and
transforming it into a “lifted” function of type `Update a msg e -> Update b msg e -> Update c msg e`.
-}
map2 : (a -> b -> p) -> Update a c e -> Update b c e -> Update p c e
map2 f =
    ap << map f


{-| Apply a function of three arguments to the state portion of a value.
-}
map3 : (a -> b -> p -> q) -> Update a c e -> Update b c e -> Update p c e -> Update q c e
map3 f x =
    ap << map2 f x


{-| Apply a function of four arguments to the state portion of a value.
-}
map4 : (a -> b -> p -> q -> r) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e
map4 f x y =
    ap << map3 f x y


{-| Apply a function of five arguments to the state portion of a value.
-}
map5 : (a -> b -> p -> q -> r -> s) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e
map5 f x y z =
    ap << map4 f x y z


{-| Apply a function of six arguments to the state portion of a value.
-}
map6 : (a -> b -> p -> q -> r -> s -> t) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e
map6 f x y z a =
    ap << map5 f x y z a


{-| Apply a function of seven arguments to the state portion of a value.
-}
map7 : (a -> b -> p -> q -> r -> s -> t -> u) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e -> Update u c e
map7 f x y z a b =
    ap << map6 f x y z a b


{-| Remove one level of monadic structure. It may suffice to know that some other functions in this library are implemented in terms of `join`. In particular, `andThen f = join << map f`
-}
join : Update (Update a c e) c e -> Update a c e
join ( ( model, cmds1, e ), cmds2, e2 ) =
    ( model, cmds1 ++ cmds2, e ++ e2 )


{-| Sequential composition of updates. This function is especially useful in combination
with the forward pipe operator (`|>`), for writing code in the style of pipelines. To chain
updates, we compose functions of the form `something -> State -> Update State msg a`:

    say : String -> State -> Update State msg a
    say what state = ...

    save state
        |> andThen (say "hello")
        |> andThen doSomethingElse

_Aside:_ `andThen` is like the monadic bind operator in Haskell, but with the arguments interchanged.

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
call, and sequentially compose them together.
-}
fold : Update a c (a -> Update a c e) -> Update a c e
fold ( m, cmd, events ) =
    List.foldr andThen ( m, cmd, [] ) events


{-| Shortcut for `\f -> fold << andThen f`
-}
foldAndThen : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
foldAndThen fun =
    fold << andThen fun


{-| Combinator useful for pointfree style. For example, if we want to make the `state` argument implicit in the following code;

    update msg state =
        case msg of
            Click ->
                setCounterValue (state.counter + 1) state

we can write:

    buttonUpdate msg =
        case msg of
            Click ->
                with .counter (setCounterValue << (+) 1)

-}
with : (a -> b) -> (b -> a -> c) -> a -> c
with get some state =
    some (get state) state


{-| See [`wrapInState`](#wrapInState) for how to work with this type.
-}
type alias WrapIn state msg state2 msg2 a =
    (state2 -> Update state2 msg2 (state -> Update state msg2 a)) -> state -> Update state msg a


{-| See [`inState`](#inState) for how to work with this type.
-}
type alias In state state2 msg a =
    WrapIn state state2 msg msg a


{-| TODO
-}
wrapInState : { get : b -> d, set : b -> m -> a, msg : c -> f } -> (d -> Update m c (a -> Update a c e)) -> b -> Update a f e
wrapInState { get, set, msg } fun state =
    get state |> fun |> foldAndThen (set state >> save) |> mapCmd msg


{-| The idea here is that you provide an `inX` for each nested `XState` that needs to be updated in your main `State`.
For instance, let's say this looks as follows;

    type State =
        { blob : Blob.State }

Then, partially applying this function, you specify a getter and setter to access `blob` within its parent record:

    inBlob : In State Blob.State msg a
    inBlob =
        inState { get = .blob, set = \state newBlob -> { state | blob = newBlob } }

Here is the `update` function

    update msg state =
        case msg of
            BlobMsg blobMsg ->
                inBlob (Blob.update blobMsg)

See the [README](https://github.com/laserpants/elm-update-deep/blob/master/README.md) file for a more thorough explanation.

-}
inState : { get : b -> d, set : b -> m -> a } -> (d -> Update m c (a -> Update a c e)) -> b -> Update a c e
inState { get, set } =
    wrapInState { get = get, set = set, msg = identity }


{-| Normally you shouldn't need to use this function directly in client code.
Instead, see the [`Update.Deep.Browser`](Update-Deep-Browser) module.
-}
applicationInit : (d -> e -> f -> Update a b c) -> d -> e -> f -> ( a, Cmd b )
applicationInit f a b c =
    let
        ( model, cmds, _ ) =
            f a b c
    in
    ( model, Cmd.batch cmds )


{-| Normally you shouldn't need to use this function directly in client code.
Instead, see the [`Update.Deep.Browser`](Update-Deep-Browser) module.
-}
documentInit : (f -> Update a b c) -> f -> ( a, Cmd b )
documentInit f a =
    let
        ( model, cmds, _ ) =
            f a
    in
    ( model, Cmd.batch cmds )


{-| Turn an update into a plain `( model, cmd )` pair, canceling any callbacks.
-}
runUpdate : (d -> e -> Update a b c) -> d -> e -> ( a, Cmd b )
runUpdate f a b =
    let
        ( model, cmds, _ ) =
            f a b
    in
    ( model, Cmd.batch cmds )
