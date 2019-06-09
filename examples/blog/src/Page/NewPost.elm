module Page.NewPost exposing (..)

import Bulma.Form exposing (controlInputModifiers, controlTextAreaModifiers)
import Bulma.Modifiers exposing (..)
import Data.Post as Post exposing (Post)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.NewPost
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Ui.Page
import Update.Deep exposing (..)
import Update.Deep.Api as Api
import Update.Deep.Form as Form

type Msg 
  = ApiMsg (Api.Msg Post)
  | FormMsg Form.Msg

type alias State =
  { api : Api.Model Post
  , formModel : Form.Model Never Form.NewPost.Fields }

inApi : In State (Api.Model Post) msg a
inApi =
    inState { get = .api, set = \state api -> { state | api = api } }

inForm : In State (Form.Model Never Form.NewPost.Fields) msg a
inForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

init : (Msg -> msg) -> Update State msg a
init toMsg = 
  let
      api = Api.init { endpoint = "/posts"
                     , method   = Api.HttpPost
                     , decoder  = Json.field "post" Post.decoder }
   in 
      save State
        |> andMap api
        |> andMap (Form.init [] Form.NewPost.validate)
        |> mapCmd toMsg

handleSubmit : (Msg -> msg) -> Form.NewPost.Fields -> State -> Update State msg a
handleSubmit toMsg form = 
  let json = form |> Form.NewPost.toJson |> Http.jsonBody 
   in inApi (Api.sendRequest "" (Just json) (toMsg << ApiMsg))

update : { onPostAdded : Post -> a } -> Msg -> (Msg -> msg) -> State -> Update State msg a
update { onPostAdded } msg toMsg = 
  case msg of
    ApiMsg apiMsg ->
      inApi (Api.update { onSuccess = invokeHandler << onPostAdded, onError = always save } apiMsg (toMsg << ApiMsg))
    FormMsg formMsg ->
      inForm (Form.update { onSubmit = handleSubmit toMsg } formMsg)

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Sub.none

view : State -> (Msg -> msg) -> Html msg
view { api, formModel } toMsg = 
  Ui.Page.container "New post" 
    [ apiResourceErrorMessage api.resource, Form.NewPost.view formModel.form formModel.disabled (toMsg << FormMsg) ]
