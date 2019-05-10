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
  | Redirect String

type alias State =
  { route : Maybe Route 
  , key   : Navigation.Key }

init : Config -> Init State Msg
init { key } =
  { route = Nothing
  , key   = key }
    |> initial

update : { onRouteChange : Maybe Route -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  case msg of
    UrlChange url ->
      let route = fromUrl url
       in { state | route = route }
        |> save
        |> andInvoke (events.onRouteChange route)
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          state
            |> runCmd (Navigation.pushUrl state.key (Url.toString url))
        Browser.External href ->
          state
            |> runCmd (Navigation.load href)
    Redirect url ->
      state
        |> runCmd (Navigation.replaceUrl state.key url)

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none
