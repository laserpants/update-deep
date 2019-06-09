module Page.Login exposing (Msg(..), State, init, subscriptions, update, view)

import Bulma.Components exposing (..)
import Bulma.Form exposing (controlInputModifiers)
import Bulma.Modifiers exposing (..)
import Data.Session as Session exposing (Session)
import Form.Login
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Update.Deep exposing (..)
import Update.Deep.Api as Api
import Update.Deep.Form as Form


type Msg
    = FormMsg Form.Msg
    | ApiMsg (Api.Msg Session)


type alias State =
    { api : Api.Model Session
    , formModel : Form.Model Never Form.Login.Fields
    }


inApi : In State (Api.Model Session) msg a
inApi =
    inState { get = .api, set = \state api -> { state | api = api } }


inForm : In State (Form.Model Never Form.Login.Fields) msg a
inForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }


init : (Msg -> msg) -> Update State msg a
init toMsg =
    let
        api =
            Api.init
                { endpoint = "/auth/login"
                , method = Api.HttpPost
                , decoder = Json.field "session" Session.decoder
                }
    in
    save State
        |> andMap api
        |> andMap (Form.init [] Form.Login.validate)
        |> mapCmd toMsg


handleSubmit : (Msg -> msg) -> Form.Login.Fields -> State -> Update State msg a
handleSubmit toMsg form =
    let
        json =
            form |> Form.Login.toJson |> Http.jsonBody
    in
    inApi (Api.sendRequest "" (Just json) (toMsg << ApiMsg))


update : { onAuthResponse : Maybe Session -> a } -> Msg -> (Msg -> msg) -> State -> Update State msg a
update { onAuthResponse } msg toMsg =
    let
        handleApiResponse maybeSession =
            inForm (Form.reset [])
                >> andInvokeHandler (onAuthResponse maybeSession)
    in
    case msg of
        ApiMsg apiMsg ->
            inApi (Api.update { onSuccess = handleApiResponse << Just, onError = handleApiResponse Nothing |> always } apiMsg (toMsg << ApiMsg))

        FormMsg formMsg ->
            inForm (Form.update { onSubmit = handleSubmit toMsg } formMsg)


subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.none


view : State -> (Msg -> msg) -> Html msg
view { api, formModel } toMsg =
    div [ class "columns is-centered is-mobile", style "margin" "6em 0" ]
        [ div [ class "column is-narrow" ]
            [ card []
                [ cardContent []
                    [ h3 [ class "title is-3" ] [ text "Log in" ]
                    , message { messageModifiers | color = Info }
                        [ style "max-width" "360px" ]
                        [ messageBody []
                            [ text "This is a demo. Log in with username 'test' and password 'test'." ]
                        ]
                    , apiResourceErrorMessage api.resource
                    , Form.Login.view formModel.form formModel.disabled (toMsg << FormMsg)
                    ]
                ]
            ]
        ]
