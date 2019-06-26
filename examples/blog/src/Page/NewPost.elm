module Page.NewPost exposing (Msg(..), State, init, subscriptions, update, view)

import Bulma.Form exposing (controlInputModifiers, controlTextAreaModifiers)
import Bulma.Modifiers exposing (..)
import Data.Post as Post exposing (Post)
import Form.Field exposing (FieldValue(..))
import Form.NewPost
import Helpers.Api exposing (resourceErrorMessage)
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
    , formModel : Form.Model Never Form.NewPost.Fields
    }


inApi : Wrap State Msg (Api.Model Post) (Api.Msg Post) a
inApi =
    wrapState
        { get = .api
        , set = \state api -> { state | api = api }
        , msg = ApiMsg
        }


inForm : Wrap State Msg (Form.Model Never Form.NewPost.Fields) Form.Msg a
inForm =
    wrapState
        { get = .formModel
        , set = \state form -> { state | formModel = form }
        , msg = FormMsg
        }


init : Update State Msg a
init =
    let
        api =
            Api.init
                { endpoint = "/posts"
                , method = Api.HttpPost
                , decoder = Json.field "post" Post.decoder
                }
    in
    save State
        |> andMap api
        |> andMap (Form.init [] Form.NewPost.validate)


handleSubmit : Form.NewPost.Fields -> State -> Update State Msg a
handleSubmit form =
    let
        json =
            form |> Form.NewPost.toJson |> Http.jsonBody
    in
    inApi (Api.sendRequest "" (Just json))


update : { onPostAdded : Post -> a } -> Msg -> State -> Update State Msg a
update { onPostAdded } msg =
    case msg of
        ApiMsg apiMsg ->
            inApi (Api.update { onSuccess = applyCallback << onPostAdded, onError = always save } apiMsg)

        FormMsg formMsg ->
            inForm (Form.update { onSubmit = handleSubmit } formMsg)


subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.none


view : State -> (Msg -> msg) -> Html msg
view { api, formModel } toMsg =
    let
        { form, disabled } =
            formModel
    in
    Ui.Page.container "New post"
        [ resourceErrorMessage api.resource
        , Form.NewPost.view form disabled (toMsg << FormMsg)
        ]
