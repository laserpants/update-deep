module App.Comments exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Comments.Form as CommentsForm exposing (Form)
import App.Config exposing (..)
import Data.Comment exposing (Comment)
import Update.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg Comment)
  | FormMsg (Form.Msg Form)

type alias State =
  { comment : Api Comment
  , form    : Form.State Form }

init : Config -> Init State Msg
init { flags } =
  let comment = Api.init { endpoint = flags.api ++ "/posts"
                         , method   = HttpPost
                         , decoder  = Json.field "comment" Data.Comment.decoder }
      form = Form.init CommentsForm.fields
                         { email   = ""
                         , comment = "" }
   in { comment = comment.state
      , form    = form.state }
        |> initial
        |> initCmd FormMsg form
        |> initCmd ApiMsg comment

onSubmit : Form -> State -> Update State Msg a
onSubmit form =
  CommentsForm.toJson form
    |> Api.jsonRequest
    |> ApiMsg
    |> update

update : Msg -> State -> Update State Msg a
update msg state =
  let resetForm = update (FormMsg Form.Reset)
      default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.comment
            |> Api.update { default | onSuccess = always resetForm } apiMsg
            |> andThen (\comment -> save { state | comment = comment })
            |> mapCmd ApiMsg
            |> consumeEvents
        FormMsg formMsg ->
          state.form
            |> Form.update { onSubmit = onSubmit } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (Form.view form)
