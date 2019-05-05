module App.Posts.Item.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Comments.Page as CommentsPage
import App.Config exposing (..)
import Data.Post exposing (Post)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg Post)
  | CommentsMsg CommentsPage.Msg

type alias State =
  { item         : Api Post
  , commentsPage : CommentsPage.State }

init : Config -> Init State Msg
init config =
  let item = Api.init { endpoint = config.flags.api ++ "/posts"
                      , method   = HttpGet
                      , decoder  = Json.field "post" Data.Post.decoder }
      commentsPage = CommentsPage.init config
   in { item         = item.state
      , commentsPage = commentsPage.state }
        |> initial
        |> initCmd ApiMsg item
        |> initCmd CommentsMsg commentsPage

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiMsg apiMsg ->
      state.item
        |> Api.update Api.defaultHandlers apiMsg
        |> andThen (\item -> save { state | item = item })
        |> mapCmd ApiMsg
        |> consumeEvents
    CommentsMsg commentsPageMsg ->
      state.commentsPage
        |> CommentsPage.update commentsPageMsg
        |> andThen (\page -> save { state | commentsPage = page })
        |> mapCmd CommentsMsg

subscriptions : State -> Sub Msg
subscriptions { commentsPage } =
  Sub.map CommentsMsg (CommentsPage.subscriptions commentsPage)

view : State -> Html Msg
view { item } = div [] []
