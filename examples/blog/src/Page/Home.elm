module Page.Home exposing (..)

import Data.Post as Post exposing (Post)
import Update.Deep.Api as Api
import Json.Decode as Json
import Update.Deep exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg 
  = HomePageApiMsg (Api.Msg (List Post))
  | FetchPosts

type alias State =
  { posts : Api.Model (List Post) }

inPosts : In State (Api.Model (List Post)) msg a
inPosts =
    inState { get = .posts, set = \state posts -> { state | posts = posts } }

init : (Msg -> msg) -> Update State msg a
init toMsg = 

  let 
      api = Api.init { endpoint = "/posts"
                     , method   = Api.HttpGet
                     , decoder  = Json.field "posts" (Json.list Post.decoder) }
   in 
      save State
        |> andMap api
        |> mapCmd toMsg

update : Msg -> (Msg -> msg) -> State -> Update State msg a
update msg toMsg = 
  case msg of
    HomePageApiMsg apiMsg ->
      inPosts (Api.update { onSuccess = always save, onError = always save } apiMsg (toMsg << HomePageApiMsg))
    FetchPosts ->
      inPosts (Api.sendSimpleRequest (toMsg << HomePageApiMsg))

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Sub.none

view : State -> (Msg -> msg) -> Html msg
view state toMsg = 

  let
      { resource } = state.posts

      postsList = 

        let listItem post =
              div [ class "content" ] 
                [ h4 [ class "title is-4" ] [ text post.title ] 
                , p [ class "content" ] [ text post.body ] 
                , p [ class "content" ] [ text (let count = List.length post.comments in if count > 0 then (String.fromInt count ++ " comment(s)") else "No comments") ] 
                , p [ class "content" ] [ a [ href ("/posts/" ++ String.fromInt post.id) ] [ text "Show post" ] ]
                ]
         in 
             case resource of
               Api.NotRequested ->
                 div [] []
               Api.Requested ->
                 div [ class "spinner" ] [ div [ class "bounce1" ] [], div [ class "bounce2" ] [], div [ class "bounce3" ] [] ]
               Api.Error error ->
                 div [] [ text "error" ]
               Api.Available posts ->
                 div [] (List.map listItem posts)
   in
      div [ class "columns is-centered", style "margin" "1.5em" ] 
        [ div [ class "column is-two-thirds" ] 
          [ h3 [ class "title is-3" ] [ text "Posts" ] 
          , postsList 
          ]
        ]
