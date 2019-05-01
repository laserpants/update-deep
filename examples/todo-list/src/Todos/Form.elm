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

update : { t | onSubmit : TodoItem -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
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
    [ div [] [ input [ onFocus OnFocus
                     , onBlur  OnBlur
                     , onInput OnChange
                     , Html.Attributes.value form.text
                     ] [] ]
    , div [] [ button [ disabled (String.isEmpty form.text)
                      , onClick OnSubmit ] [ text "Submit" ] ] ]
