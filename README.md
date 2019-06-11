# Update Deep

## Examples

You may find the following examples enlightening:

### [Facepalm](https://laserpants.github.io/elm-update-deep/examples/blog/)

This is a simple single-page (SPA) blog app, showing how to use this library to:
  * Fetch remote resources from an API; 
  * Implement URL routing; 
  * Do authentication and manage sessions using localStorage/sessionStorage; 
  * Display “toast” notifications; and
  * Work with 
    * forms (wrapping [elm-form](https://package.elm-lang.org/packages/etaque/elm-form/latest)) and 
    * WebSockets (see Register page).

### [Trollo](https://laserpants.github.io/elm-update-deep/examples/todo-list/) 

An even simpler todo-list application, explained in more detail in [this README file](https://github.com/laserpants/elm-update-deep/tree/master/examples/todo-list).

## Intro

In a nutshell, this library let's you do the following:

1) Chain updates conveniently using the pipes operator (similar to `update-extra`):

```elm
 model
     |> setResource Requested
     |> andAddCmd (model.request url maybeBody)
     |> mapCmd toMsg
```

2) Allow for information to be passed *up* in the update tree:

```elm
 model
     |> setResource (Available resource)
     |> andApplyCallback (onSuccess resource) -- pass the message
```

3) Reduce boilerplate when working with nested updates:

```elm
 type State =
     { router : Router.State }

 type Msg
     = RouterMsg Router.Msg
     | UiMsg Ui.Msg

 update msg =
     case msg of
         RouterMsg routerMsg ->
             inRouter (Router.update routerMsg RouterMsg)
```

## Hello, world

Let's look at an example:

```
        ┌───────────┐    
        │   State   │    
        └──┬─── ▲ ──┘    
           │    │        
 ButtonMsg │    │--- buttonClicked
           │    │        
      ┌─── ▼ ───┴─────┐   
      │  ButtonState  │   
      └───────────────┘   
```

As much as possible, we'd like to encapsulate the button functionality in a 
stand-alone *component* (reusability and all that).

```elm
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
                |> andThen (applyCallback (buttonClicked count))


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

A reasonable next step here is to break this up into separate modules, so that `ButtonMsg` becomes `Button.Msg`, etc. Look at the other two [example applications](https://github.com/laserpants/elm-update-deep/tree/master/examples) to see how that can be done.

Note that the `update` function is written in pointfree style. Making the `state` argument explicit, the same code translates to:

```elm
update : Msg -> State -> Update State Msg a
update msg state =
    case msg of
        ButtonMsg buttonMsg ->
            state
                |> inButton (buttonUpdate { buttonClicked = handleButtonClicked } buttonMsg)
```

The `inButton` function takes care of some wrapping and unwrapping of the record for us. To examine what it does, we could expand this even further, into the following:

```elm
update : Msg -> State -> Update State Msg a
update msg state =
    case msg of
        ButtonMsg buttonMsg ->
            state.button
                |> buttonUpdate { buttonClicked = handleButtonClicked } buttonMsg
                |> andThen (\button -> save { state | button = button })
                |> fold
```

The idea here is that you provide an `inX` for each nested `XState` that needs to be updated.

```
type State =
  { x : XState }
```

Partially applying `inState`, you need to specify a getter and setter to access `x` within the parent record:

```
inX = inState { get = .x, set = \state newX -> { state | x = newX } }
```

## Hello, HTTP world

