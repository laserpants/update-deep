module App.Posts.List.Page exposing (..)

import Api exposing (Api, HttpMethod(..), Resource(..))
import App.Config exposing (..)
import Data.Post exposing (Post)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg (List Post))

type alias State =
  { collection : Api (List Post) }

init : Config -> Init State Msg
init { flags } =
  let collection = Api.init { endpoint = flags.api ++ "/posts"
                            , method   = HttpGet
                            , decoder  = Json.field "posts" (Json.list Data.Post.decoder) }
   in { collection = collection.state }
        |> initial
        |> initCmd ApiMsg collection

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiMsg apiMsg ->
      state.collection
        |> Api.update Api.defaultHandlers apiMsg
        |> andThen (\collection -> save { state | collection = collection })
        |> mapCmd ApiMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { collection } =
  case collection.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
    Requested ->
      div [] [ text "Requested..." ]
    Error error ->
      div [] [ text "Error" ]
    Available posts ->
      div [] (List.map (\_ -> div [] [ text "item" ]) posts)
