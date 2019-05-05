module App.Posts.Create.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import App.Posts.Create.Form as CreateForm exposing (Form)
import Data.Post exposing (Post)
import FormState exposing (..)
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

onSubmit : Form -> State -> Update State Msg a
onSubmit form =
  CreateForm.toJson form
    |> Api.jsonRequest
    |> ApiMsg
    |> update

update : Msg -> State -> Update State Msg a
update msg state =
  let resetForm = update (FormMsg FormState.Reset)
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
            |> FormState.update { onSubmit = onSubmit } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none
