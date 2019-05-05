module App.Posts exposing (..)

import Api exposing (Api, HttpMethod(..), Resource(..))
import App.Config exposing (..)
import App.Posts.Form as PostsForm exposing (Form)
import Data.Post exposing (Post)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = ApiPostsMsg (Api.Msg (List Post))
  | ApiCreatePostMsg (Api.Msg Post)
  | ApiFetchPostMsg (Api.Msg Post)
  | FormMsg (FormState.Msg Form)
  | FetchPosts

type alias State =
  { collection : Api (List Post)
  , postCreate : Api Post
  , postFetch  : Api Post
  , form       : FormState Form }

init : Config -> Init State Msg
init { flags } =
  let endpoint   = flags.api ++ "/posts"
      form       = FormState.init PostsForm.fields { title = "", body = "" }
      postCreate = Api.init { endpoint = endpoint
                            , method   = HttpPost
                            , decoder  = Json.field "post" Data.Post.decoder }
      postFetch  = Api.init { endpoint = endpoint
                            , method   = HttpGet
                            , decoder  = Json.field "post" Data.Post.decoder }
      collection = Api.init { endpoint = endpoint
                            , method   = HttpGet
                            , decoder  = Json.field "posts" (Json.list Data.Post.decoder) }
   in { collection = collection.state
      , postCreate = postCreate.state
      , postFetch  = postFetch.state
      , form       = form.state }
        |> initial
        |> initCmd ApiPostsMsg collection
        |> initCmd ApiCreatePostMsg postCreate
        |> initCmd ApiFetchPostMsg postFetch
        |> initCmd FormMsg form

sendCreatePostRequest : Json.Value -> State -> Update State Msg a
sendCreatePostRequest json = update (ApiCreatePostMsg (Api.jsonRequest json))

resetCreatePostForm : State -> Update State Msg a
resetCreatePostForm = update (FormMsg (FormState.Reset))

fetchCollectionMsg : Msg
fetchCollectionMsg = ApiPostsMsg (Api.Request Nothing)

fetchPostMsg : Int -> Msg
fetchPostMsg id = ApiFetchPostMsg (Api.RequestResource ("/" ++ String.fromInt id) Nothing)

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiPostsMsg apiMsg ->
      state.collection
        |> Api.update { onSuccess = save, onError = \error -> save } apiMsg
        |> andThen (\posts -> save { state | collection = posts })
        |> mapCmd ApiPostsMsg
        |> consumeEvents
    ApiCreatePostMsg apiMsg ->
      state.postCreate
        |> Api.update { onSuccess = resetCreatePostForm
                      , onError   = \error -> resetCreatePostForm } apiMsg
        |> andThen (\post -> save { state | postCreate = post })
        |> mapCmd ApiCreatePostMsg
        |> consumeEvents
    ApiFetchPostMsg apiMsg ->
      state.postFetch
        |> Api.update { onSuccess = save, onError = \error -> save } apiMsg
        |> andThen (\post -> save { state | postFetch = post })
        |> mapCmd ApiFetchPostMsg
        |> consumeEvents
    FormMsg formMsg ->
      state.form
        |> FormState.update { onSubmit = \form -> sendCreatePostRequest (PostsForm.toJson form) } formMsg
        |> andThen (\form -> save { state | form = form })
        |> mapCmd FormMsg
        |> consumeEvents
    FetchPosts ->
      state
        |> update (ApiPostsMsg (Api.Request Nothing))

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

formView : State -> Html Msg
formView { form } = Html.map FormMsg (FormState.view form)

postView : Post -> Html Msg
postView { id, title, body } =
  let postUrl = "posts/" ++ String.fromInt id
   in div []
    [ h2 [] [ text title ]
    , p [] [ text body ]
    , a [ href postUrl ] [ text "Show" ]
    , text " | "
    , a [ href (postUrl ++ "/comments/new") ]
        [ text "Comment" ] ]

listView : State -> Html Msg
listView { collection } =
  case collection.resource of
    NotRequested ->
      div [] [ text "Not requested"
             , button [ onClick FetchPosts ] [ text "Fetch" ] ]
    Requested ->
      div [] [ text "Requested..." ]
    Error error ->
      div [] [ text "Error" ]
    Available posts ->
      div [] (List.map postView posts)

itemView : State -> Html Msg
itemView { postFetch } =
  case postFetch.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
    Requested ->
      div [] [ text "Fetching..." ]
    Error (Http.BadStatus 404) ->
      div [] [ text "That post was not found" ]
    Error error ->
      div [] [ text "Error", text (Debug.toString error) ]
    Available post ->
      div []
        [ div [] [ text "Post item" ]
        , div [] [ a [ href ("/posts/" ++ String.fromInt post.id ++ "/comments/new") ] [ text "Comment" ] ] ]
