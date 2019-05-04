module App.Auth exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Config exposing (..)
import Data.User as User exposing (User)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import LoginForm exposing (..)
import Update.Deep exposing (..)

type Msg
  = ApiUserMsg (Api.Msg User)
  | FormMsg (FormState.Msg LoginForm)

type alias State =
  { user  : Api User
  , login : FormState LoginForm }

init : Config -> Init State Msg
init { flags } =
  let user = Api.init { endpoint = flags.api ++ "/auth/login"
                      , method   = Post
                      , decoder  = Json.field "user" User.decoder }
      login = FormState.init fields { login = "", password = "" }
   in { user  = user.state
      , login = login.state }
        |> initial
        |> initCmd ApiUserMsg user
        |> initCmd FormMsg login

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
    FormMsg formMsg ->
      state.login
        |> FormState.update { onSubmit = \form -> sendAuthRequest (LoginForm.toJson form) } formMsg
        |> andThen (\form -> save { state | login = form })
        |> mapCmd FormMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

loginForm : State -> Html Msg
loginForm state = Html.map FormMsg (FormState.view state.login)
