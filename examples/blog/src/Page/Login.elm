module Page.Login exposing (Msg(..), State, init, subscriptions, update, view)

import Bulma.Components exposing (..)
import Bulma.Form exposing (controlInputModifiers)
import Bulma.Modifiers exposing (..)
import Data.Session as Session exposing (Session)
import Form.Login
import Helpers.Api exposing (resourceErrorMessage)
import Helpers.Form exposing (..)
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


inApi : Wrap State Msg (Api.Model Session) (Api.Msg Session) a
inApi =
    wrapState
        { get = .api
        , set = \state api -> { state | api = api }
        , msg = ApiMsg
        }


inForm : Wrap State Msg (Form.Model Never Form.Login.Fields) Form.Msg a
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
                { endpoint = "/auth/login"
                , method = Api.HttpPost
                , decoder = Json.field "session" Session.decoder
                }
    in
    save State
        |> andMap api
        |> andMap (Form.init [] Form.Login.validate)


handleSubmit : Form.Login.Fields -> State -> Update State Msg a
handleSubmit form =
    let
        json =
            form |> Form.Login.toJson |> Http.jsonBody
    in
    inApi (Api.sendRequest "" (Just json))


update : { onAuthResponse : Maybe Session -> a } -> Msg -> State -> Update State Msg a
update { onAuthResponse } msg =
    let
        handleApiResponse maybeSession =
            inForm (Form.reset [])
                >> andApplyCallback (onAuthResponse maybeSession)
    in
    case msg of
        ApiMsg apiMsg ->
            inApi (Api.update { onSuccess = handleApiResponse << Just, onError = handleApiResponse Nothing |> always } apiMsg)

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
                    , resourceErrorMessage api.resource
                    , Form.Login.view form disabled (toMsg << FormMsg)
                    ]
                ]
            ]
        ]
