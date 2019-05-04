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
  | FormMsg (FormState.Msg Form)
  | FetchPosts

type alias State =
  { collection : Api (List Post)
  , post       : Api Post
  , form       : FormState Form }

init : Config -> Init State Msg
init { flags } =
  let form       = FormState.init PostsForm.fields { title = "", body = "" }
      post       = Api.init { endpoint = flags.api ++ "/posts"
                            , method   = Api.Post
                            , decoder  = Json.field "post" Data.Post.decoder }
      collection = Api.init { endpoint = flags.api ++ "/posts"
                            , method   = Get
                            , decoder  = Json.field "posts" (Json.list Data.Post.decoder) }
   in { collection = collection.state
      , post       = post.state
      , form       = form.state }
        |> initial
        |> initCmd ApiPostsMsg collection
        |> initCmd ApiCreatePostMsg post
        |> initCmd FormMsg form

sendCreatePostRequest : Json.Value -> State -> Update State Msg a
sendCreatePostRequest json = update (ApiCreatePostMsg (Api.jsonRequest json))

resetCreatePostForm : State -> Update State Msg a
resetCreatePostForm = update (FormMsg (FormState.Reset))

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
      state.post
        |> Api.update { onSuccess = resetCreatePostForm
                      , onError   = \error -> resetCreatePostForm }
                      apiMsg
        |> andThen (\post -> save { state | post = post })
        |> mapCmd ApiCreatePostMsg
        |> consumeEvents
    FormMsg formMsg ->
      state.form
        |> FormState.update { onSubmit = \form -> sendCreatePostRequest (PostsForm.toJson form) } formMsg
        |> andThen (\form -> save { state | form = form })
        |> mapCmd FormMsg
        |> consumeEvents
    FetchPosts ->
      state
        |> update (ApiPostsMsg Api.SimpleRequest)

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

formView : State -> Html Msg
formView { form } = Html.map FormMsg (FormState.view form)

postView : Post -> Html Msg
postView { id, title, body } =
  div []
    [ h2 [] [ text title ]
    , p [] [ text body ]
    , a [ href ("posts/" ++ String.fromInt id ++ "/comments/new") ]
        [ text "Comment" ] ]

listView : State -> Html Msg
listView { collection } =
  case collection.resource of
    NotRequested ->
      div [] [ text "Not requested"
             , button [ onClick FetchPosts ] [ text "Fetch" ] ]
    Requested ->
      div [] [ text "Requested" ]
    Error error ->
      div [] [ text "Error" ]
    Available posts ->
      div [] (List.map postView posts)
