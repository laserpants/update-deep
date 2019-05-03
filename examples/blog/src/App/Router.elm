module App.Router exposing (..)

import App.Config exposing (..)
import App.Route exposing (..)
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest

type alias State =
  { route : Maybe Route 
  , key   : Navigation.Key }

init : Config -> Init State Msg
init config =
  { route = Nothing 
  , key   = config.key }
    |> initial

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    UrlChange url ->
      save { state | route = fromUrl url }
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          state
            |> runCmd (Navigation.pushUrl state.key (Url.toString url))
        Browser.External href ->
          state
            |> runCmd (Navigation.load href)

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none
