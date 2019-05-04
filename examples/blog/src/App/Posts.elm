module App.Posts exposing (..)

import App.Config exposing (..)
import App.Posts.Form as PostsForm exposing (Form)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = FormMsg (FormState.Msg Form)

type alias State =
  { form : FormState Form }

init : Config -> Init State Msg
init config =
  let form = FormState.init PostsForm.fields { title = "", body = "" }
   in { form = form.state }
        |> initial

update : Msg -> State -> Update State Msg a
update msg state =
  save state

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

formView : State -> Html Msg
formView { form } = Html.map FormMsg (FormState.view form)
