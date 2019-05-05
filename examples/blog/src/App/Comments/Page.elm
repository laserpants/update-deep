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
  = SetPost (Maybe Post)
  | FormMsg (FormState.Msg Form)

type alias State =
  { post : Maybe Post
  , form : FormState Form }

init : Config -> Init State Msg
init { flags } =
  let form = FormState.init CommentsForm.fields
                 { email   = ""
                 , comment = "" }
   in { post = Nothing
      , form = form.state }
        |> initial
        |> initCmd FormMsg form

update : Msg -> State -> Update State Msg a
update msg state =
  let resetForm = update (FormMsg FormState.Reset)
      default = Api.defaultHandlers
   in case msg of
        FormMsg formMsg ->
          state.form
            |> FormState.update defaultHandlers formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents
        SetPost post ->
          save { state | post = post }

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (FormState.view form)
