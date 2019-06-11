module Main exposing (ButtonMsg(..), ButtonState, Msg(..), State, buttonInit, buttonUpdate, buttonView, handleButtonClicked, init, main, setCounterValue, update, view)

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



--


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
