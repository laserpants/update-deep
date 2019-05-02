module Todos.Form exposing (..)

import Data.TodoItem exposing (..)
import Update.Deep exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg
  = OnSubmit
  | OnFocus
  | OnBlur
  | OnChange String

type alias State =
  { text : String }

setText : String -> State -> Update State Msg a
setText text state = save { state | text = text }

type alias EventHandlers t a c e =
  { t | onSubmit : TodoItem -> a -> Update a c e }

update : EventHandlers t a c e -> Msg -> State -> Update State Msg (a -> Update a c e)
update { onSubmit } msg state =
  case msg of
    OnSubmit ->
      state
        |> setText ""
        |> andInvoke (onSubmit { text = state.text })
    OnFocus ->
      save state
    OnBlur ->
      save state
    OnChange text ->
      state
        |> setText text

init : Init State Msg
init = initial { text = "" }

view : State -> Html Msg
view form =
  div []
    [ h3 [ style "margin" ".5em 0" ] [ text "New task" ]
    , div [] [ input [ onFocus OnFocus
                     , onBlur  OnBlur
                     , onInput OnChange
                     , Html.Attributes.value form.text
                     ] [] ]
    , div [ style "margin" ".5em 0" ]
        [ button [ disabled (String.isEmpty form.text)
                 , onClick OnSubmit ] [ text "Submit" ] ] ]
