module App.Posts.Create exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import App.Posts.Create.Form as CreateForm exposing (Form)
import Data.Post exposing (Post)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Update.Deep exposing (..)
import Update.Form as Form

type Msg
  = ApiMsg (Api.Msg Post)
  | FormMsg (Form.Msg Form)

type alias State =
  { post : Api Post
  , form : Form.State Form }

init : Config -> Init State Msg
init { flags } =
  let post = Api.init { endpoint = flags.api ++ "/posts"
                      , method   = HttpPost
                      , decoder  = Json.field "post" Data.Post.decoder }
      form = Form.init CreateForm.fields { title = "", body = "" }
   in { post = post.state
      , form = form.state }
        |> initial
        |> initCmd ApiMsg post
        |> initCmd FormMsg form

onSubmit : { onPostAdded : Post -> a -> Update a c e } -> Form -> State -> Update State Msg (a -> Update a c e)
onSubmit events form =
  form
    |> CreateForm.toJson
    |> Api.jsonRequest
    |> ApiMsg
    |> update events

onSuccess : { onPostAdded : Post -> a -> Update a c e } -> Post -> State -> Update State Msg (a -> Update a c e)
onSuccess events post state =
  let resetForm = update events (FormMsg Form.Reset)
   in state
    |> invoke (events.onPostAdded post)
    |> andThen resetForm

update : { onPostAdded : Post -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  let default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.post
            |> Api.update { default | onSuccess = onSuccess events } apiMsg
            |> andThen (\post -> save { state | post = post })
            |> mapCmd ApiMsg
            |> consumeEvents
        FormMsg formMsg ->
          state.form
            |> Form.update { onSubmit = onSubmit events } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (Form.view form)
