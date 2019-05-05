module App.Posts.Item.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import Data.Post exposing (Post)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg Post)

type alias State =
  { item : Api Post }

init : Config -> Init State Msg
init { flags } =
  let item = Api.init { endpoint = flags.api ++ "/posts"
                      , method   = HttpGet
                      , decoder  = Json.field "post" Data.Post.decoder }

   in { item = item.state }
        |> initial
        |> initCmd ApiMsg item

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiMsg apiMsg ->
      state.item
        |> Api.update Api.defaultHandlers apiMsg
        |> andThen (\item -> save { state | item = item })
        |> mapCmd ApiMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { item } = div [] []
