module Todos.Form exposing (FormData, Msg(..), State, init, setText, update, view)

import Data.TodoItem exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)


type Msg
    = Submit
    | Focus
    | Blur
    | Change String


type alias State =
    { text : String }


setText : String -> State -> Update State msg a
setText text state =
    save { state | text = text }


type alias FormData =
    { text : String }


update : { onSubmit : FormData -> a } -> Msg -> State -> Update State msg a
update { onSubmit } msg state =
    case msg of
        Submit ->
            state
                |> invokeHandler (onSubmit { text = state.text })
                |> andThen (setText "")

        Focus ->
            save state

        Blur ->
            save state

        Change text ->
            state
                |> setText text


init : (Msg -> msg) -> Update State msg a
init toMsg =
    save State
        |> andMap (save "")
        |> mapCmd toMsg


view : State -> (Msg -> msg) -> Html msg
view { text } toMsg =
    div [ class "box" ]
        [ h4 [ class "title is-4" ]
            [ Html.text "New task" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ Html.text "Description" ]
            , div [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Describe your task here"
                    , onFocus (toMsg Focus)
                    , onBlur (toMsg Blur)
                    , onInput (toMsg << Change)
                    , Html.Attributes.value text
                    ]
                    []
                ]
            ]
        , div []
            [ button
                [ class "button"
                , disabled (String.isEmpty text)
                , onClick (toMsg Submit)
                ]
                [ Html.text "Add" ]
            ]
        ]
