module App.Ui exposing (..)

import App.Config exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = NoMsg

type alias State =
  {}

init : Config -> Init State Msg
init config =
  initial {}

update : Msg -> State -> Update State Msg a
update msg state =
  save state

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none
