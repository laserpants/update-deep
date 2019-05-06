module App.Posts.Create.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import App.Posts.Create.Form as CreateForm exposing (Form)
import Data.Post exposing (Post)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg Post)
  | FormMsg (FormState.Msg Form)

type alias State =
  { post : Api Post
  , form : FormState Form }

init : Config -> Init State Msg
init { flags } =
  let post = Api.init { endpoint = flags.api ++ "/posts"
                      , method   = HttpPost
                      , decoder  = Json.field "post" Data.Post.decoder }
      form = FormState.init CreateForm.fields { title = "", body = "" }
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

update : { onPostAdded : Post -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  let resetForm = update events (FormMsg FormState.Reset)
      default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.post
            |> Api.update { default | onSuccess = resetForm } apiMsg
            |> andThen (\post -> save { state | post = post })
            |> mapCmd ApiMsg
            |> consumeEvents
        FormMsg formMsg ->
          state.form
            |> FormState.update { onSubmit = onSubmit events } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (FormState.view form)
