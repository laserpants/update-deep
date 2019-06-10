module Update.Deep exposing
    ( In, Update
    , addCmd, andMap, andThen, andThenIf, ap, applicationInit, documentInit, foldEvents, foldEventsAndThen, inState, invokeHandler, join, kleisli, map, map2, map3, map4, map5, map6, map7, mapCmd, runUpdate, save, unwrap
    , andAddCmd, andInvokeHandler
    )

{-|

# Examples/Tutorial

### [Facepalm](https://laserpants.github.io/elm-update-deep/examples/blog/)

A simple blog application, demonstrating how to use this library to:
  * fetch remote resources from a (fake) API; 
  * do URL routing; 
  * manage sessions using localStorage and sessionStorage; 
  * work with 
    * forms (using elm-form) and 
    * WebSockets.

### [Trollo](https://laserpants.github.io/elm-update-deep/examples/todo-list/) 

An even simpler todo-list application, explained in [this README file](https://github.com/laserpants/elm-update-deep/tree/master/examples/todo-list).

# Intro

In a nutshell, this library let's you do the following:

1) Chain updates using the pipes operator (similar to `update-extra`):

```
    model
        |> setResource Requested
        |> andAddCmd (model.request url maybeBody)
        |> mapCmd toMsg
```

2) Allow information to be passed up in the update tree:

```
    model
        |> setResource (Available resource)
        |> andInvokeHandler (onSuccess resource)
```

3) Reduce boilerplate while working with nested updates:

```
    type Msg
        = RouterMsg Router.Msg
        | UiMsg Ui.Msg

    update msg =
        case msg of
            RouterMsg routerMsg ->
                inRouter (Router.update { onRouteChange = handleRouteChange } routerMsg)
```

## Hello, world

Let's look at a minimal example:


```
module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Update.Deep.Browser as Deep


type ButtonMsg
    = Click


type alias ButtonState =
    { counter : Int }


setCounterValue : Int -> ButtonState -> Update ButtonState msg a
setCounterValue count state =
    save { state | counter = count }


buttonInit : Update ButtonState msg a
buttonInit =
    save ButtonState
        |> andMap (save 0)


buttonUpdate : { buttonClicked : Int -> a } -> ButtonMsg -> ButtonState -> Update ButtonState msg a
buttonUpdate { buttonClicked } msg state =
    case msg of
        Click ->
            let
                count =
                    1 + state.counter
            in
            state
                |> setCounterValue count
                |> andThen (invokeHandler (buttonClicked count))


buttonView : (ButtonMsg -> msg) -> Html msg
buttonView toMsg =
    div [] [ button [ onClick Click ] [ text "Click me" ] ]
        |> Html.map toMsg



-- Main application


type Msg
    = ButtonMsg ButtonMsg


type alias State =
    { button : ButtonState
    , message : String
    }


init : () -> Update State Msg a
init () =
    save State
        |> andMap buttonInit
        |> andMap (save "")


handleButtonClicked : Int -> State -> Update State msg a
handleButtonClicked times state =
    save { state | message = "The button has been clicked " ++ String.fromInt times ++ " time(s)." }


inButton : In State ButtonState msg a
inButton =
    inState { get = .button, set = \state button -> { state | button = button } }


update : Msg -> State -> Update State Msg a
update msg =
    case msg of
        ButtonMsg buttonMsg ->
            inButton (buttonUpdate { buttonClicked = handleButtonClicked } buttonMsg)


view : State -> Document Msg
view { message } =
    { title = ""
    , body = [ buttonView ButtonMsg, text message ]
    }


main : Program () State Msg
main =
    Deep.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
```

then


```
update : Msg -> State -> Update State Msg a
update msg state =
    case msg of
        ButtonMsg buttonMsg ->
            state.button
                |> buttonUpdate { buttonClicked = handleButtonClicked } buttonMsg
                |> andThen (\button -> save { state | button = button })
                |> foldEvents
```


# The Update Type

@docs Update, save, addCmd, map, mapCmd, invokeHandler, foldEvents, join, kleisli


## Chaining Updates

@docs andThen, andThenIf


## Applicative Interface

@docs andMap, ap, map2, map3, map4, map5, map6, map7


# Nested State

@docs In, inState


# Program Integration

@docs runUpdate, applicationInit, documentInit


# Helpers

@docs andAddCmd, andInvokeHandler, foldEventsAndThen, unwrap

-}


{-| A type alias wrapper for Elm's `( model, Cmd msg )` tuple that introduces a
third component to allow for one or more callbacks to be passed down in the update hierarchy.
-}
type alias Update m c e =
    ( m, Cmd c, List e )


{-| This function lifts a value into the `Update` context. For example, 

```
save someState
```

is the same as `( someState, Cmd.none )` in code that doesn't use this library.
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

See also [`andAddCmd`](#andAddCmd).

-}
addCmd : Cmd c -> m -> Update m c e
addCmd cmd state =
    ( state, cmd, [] )


{-| Apply a function to the command part of the value. This is typically used to lift a
value returned from a nested update into the caller's context. For example;

    type alias State =
        { enchilada : String }

    init : (Msg -> msg) -> Update State msg a
    init toMsg =
        save State
            |> andMap (save "")
            |> mapCmd toMsg
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


{-| Removes one level of monadic structure. Various other functions in this library are implemented in terms of `join`. In particular, `andThen f = join << map f`
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
