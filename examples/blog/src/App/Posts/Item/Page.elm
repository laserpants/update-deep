module App.Posts.Item.Page exposing (..)

import Api exposing (Api, HttpMethod(..), Resource(..))
import App.Comments.Page as CommentsPage
import App.Config exposing (..)
import Data.Post exposing (Post)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg Post)
  | CommentsMsg CommentsPage.Msg
  | SetPost Int

type alias State =
  { post         : Api Post
  , postId       : Maybe Int
  , commentsPage : CommentsPage.State }

init : Config -> Init State Msg
init config =
  let post = Api.init { endpoint = config.flags.api ++ "/posts"
                      , method   = HttpGet
                      , decoder  = Json.field "post" Data.Post.decoder }
      commentsPage = CommentsPage.init config
   in { post         = post.state
      , postId       = Nothing
      , commentsPage = commentsPage.state }
        |> initial
        |> initCmd ApiMsg post
        |> initCmd CommentsMsg commentsPage

setPost : Int -> State -> Update State Msg a
setPost id state =
  let url = "/" ++ String.fromInt id
      requestPost = update (ApiMsg (Api.RequestUrl url Nothing))
   in
      if state.postId == Just id
          then
            case state.post.resource of
              Requested ->
                save state
              Available _ ->
                save state
              _ ->
                state |> requestPost
          else
            { state | postId = Just id } |> requestPost

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiMsg apiMsg ->
      state.post
        |> Api.update Api.defaultHandlers apiMsg
        |> andThen (\post -> save { state | post = post })
        |> mapCmd ApiMsg
        |> consumeEvents
    CommentsMsg commentsPageMsg ->
      state.commentsPage
        |> CommentsPage.update commentsPageMsg
        |> andThen (\page -> save { state | commentsPage = page })
        |> mapCmd CommentsMsg
    SetPost id ->
      state
        |> setPost id

subscriptions : State -> Sub Msg
subscriptions { commentsPage } =
  Sub.map CommentsMsg (CommentsPage.subscriptions commentsPage)

view : State -> Html Msg
view { post } =
  case post.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
    Requested ->
      div [] [ text "Requested..." ]
    Error (Http.BadStatus 404) ->
      div [] [ text "Not found" ]
    Error error ->
      div [] [ text "Error" ]
    Available { id, title, body } ->
      div []
        [ h1 [] [ text title ]
        , p [] [ text body ]
        , p [] [ a [ href ("/posts/" ++ String.fromInt id ++ "/comments/new") ] [ text "Comment" ] ]
        ]
