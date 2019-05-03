module App.Auth exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import Data.User as User exposing (User)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = ApiUserMsg (Api.Msg User)
  | LoginFormMsg Form.Msg

type alias State =
  { user  : Api User
  , login : Form }

init : Config -> Init State Msg
init { flags } =
  let user  = Api.init { endpoint = flags.api ++ "/auth/login"
                       , method   = Post
                       , decoder  = Json.field "user" User.decoder }
      login = Form.init
   in { user  = user.state
      , login = login.state }
        |> initial
        |> initCmd ApiUserMsg user
        |> initCmd LoginFormMsg login

sendAuthRequest : Json.Value -> State -> Update State Msg a
sendAuthRequest json =
  let request = Api.Request (Just (Http.jsonBody json))
   in update (ApiUserMsg request)

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiUserMsg apiMsg ->
      state.user
        |> Api.update apiMsg
        |> andThen (\user -> save { state | user = user })
        |> mapCmd ApiUserMsg
    LoginFormMsg formMsg ->
      state.login
        |> Form.update { onSubmit = \form -> sendAuthRequest (Form.toJson form) } formMsg
        |> andThen (\form -> save { state | login = form })
        |> mapCmd LoginFormMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

loginForm : State -> Html Msg
loginForm state =
  div [ onClick (LoginFormMsg Form.Submit) ] [ button [] [ text "Submit" ] ]
