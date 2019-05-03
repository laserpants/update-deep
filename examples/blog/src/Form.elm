module Form exposing (..)

import Json.Encode as Encode exposing (Value, object)
import Update.Deep exposing (..)

type Msg
  = Submit
  | Reset

type alias Form =
  {}

init : Init Form Msg
init =
  initial {}

type alias Events t a c e = { t | onSubmit : Form -> a -> Update a c e }

update : Events t a c e -> Msg -> Form -> Update Form Msg (a -> Update a c e)
update { onSubmit } msg state =
  case msg of
    Submit ->
      state
        |> invoke (onSubmit state)
    Reset ->
      save {}

-- TODO
toJson : Form -> Value
toJson _ = object [ ( "login", Encode.string "test" ), ( "password", Encode.string "test" ) ]
