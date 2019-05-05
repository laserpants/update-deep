module App.Comments.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Comments.Form as CommentsForm exposing (Form)
import App.Config exposing (..)
import Data.Post exposing (Post)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
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
                      , method   = HttpGet
                      , decoder  = Json.field "post" Data.Post.decoder }
      form = FormState.init CommentsForm.fields
                      { email   = ""
                      , comment = "" }
   in { post = post.state 
      , form = form.state }
        |> initial
        |> initCmd ApiMsg post
        |> initCmd FormMsg form

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
            |> FormState.update defaultHandlers formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (FormState.view form)
