module App.Auth exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Auth.LoginForm as LoginForm exposing (LoginForm)
import App.Auth.RegistrationForm as RegistrationForm exposing (RegistrationForm)
import App.Config exposing (..)
import Data.User as User exposing (User)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Update.Deep exposing (..)

type Msg
  = ApiUserMsg (Api.Msg User)
  | LoginFormMsg (FormState.Msg LoginForm)
  | RegistrationFormMsg (FormState.Msg RegistrationForm)

type alias State =
  { user         : Api User
  , login        : FormState LoginForm
  , registration : FormState RegistrationForm }

init : Config -> Init State Msg
init { flags } =
  let user = Api.init { endpoint = flags.api ++ "/auth/login"
                      , method   = Post
                      , decoder  = Json.field "user" User.decoder }
      login = FormState.init LoginForm.fields { login = "", password = "" }
      registration = FormState.init RegistrationForm.fields { login = "", password = "" }
   in { user         = user.state
      , login        = login.state
      , registration = registration.state }
        |> initial
        |> initCmd ApiUserMsg user
        |> initCmd LoginFormMsg login
        |> initCmd RegistrationFormMsg registration

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
        |> FormState.update { onSubmit = \form -> sendAuthRequest (LoginForm.toJson form) } formMsg
        |> andThen (\form -> save { state | login = form })
        |> mapCmd LoginFormMsg
        |> consumeEvents
    RegistrationFormMsg formMsg ->
      state.registration
        |> FormState.update { onSubmit = \form -> sendAuthRequest (RegistrationForm.toJson form) } formMsg
        |> andThen (\form -> save { state | registration = form })
        |> mapCmd RegistrationFormMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

loginForm : State -> Html Msg
loginForm { login } = Html.map LoginFormMsg (FormState.view login)

registrationForm : State -> Html Msg
registrationForm { registration } = Html.map RegistrationFormMsg (FormState.view registration)
