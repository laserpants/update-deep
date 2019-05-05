module App.Auth exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Auth.Login.Form as LoginForm exposing (LoginForm)
import App.Auth.Login.Page as LoginPage
import App.Auth.Register.Form as RegisterForm exposing (RegisterForm)
import App.Auth.Register.Page as RegisterPage
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
  | ApiRegisterMsg (Api.Msg { status : String })
  | LoginFormMsg (FormState.Msg LoginForm)
  | RegisterFormMsg (FormState.Msg RegisterForm)

type alias State =
  { user         : Api User
  , registration : Api { status : String }
  , loginForm    : FormState LoginForm
  , registerForm : FormState RegisterForm }

init : Config -> Init State Msg
init { flags } =
  let endpoint     = flags.api ++ "/auth"
      user         = Api.init { endpoint = endpoint ++ "/login"
                              , method   = HttpPost
                              , decoder  = Json.field "user" User.decoder }
      registration = Api.init { endpoint = endpoint ++ "/register"
                              , method   = HttpPost
                              , decoder  = Json.map (\status -> { status = status }) (Json.field "status" Json.string) }
      loginForm    = FormState.init LoginForm.fields
                              { login    = ""
                              , password = "" }
      registerForm = FormState.init RegisterForm.fields
                              { login    = ""
                              , password = "" }
   in { user         = user.state
      , registration = registration.state
      , loginForm    = loginForm.state
      , registerForm = registerForm.state }
        |> initial
        |> initCmd ApiUserMsg user
        |> initCmd LoginFormMsg loginForm
        |> initCmd RegisterFormMsg registerForm

sendAuthRequest : Json.Value -> State -> Update State Msg a
sendAuthRequest json = update (ApiUserMsg (Api.jsonRequest json))

sendRegisterRequest : Json.Value -> State -> Update State Msg a
sendRegisterRequest json = update (ApiRegisterMsg (Api.jsonRequest json))

resetLoginForm : State -> Update State Msg a
resetLoginForm = update (LoginFormMsg FormState.Reset)

resetRegisterForm : State -> Update State Msg a
resetRegisterForm = update (RegisterFormMsg FormState.Reset)

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ApiUserMsg apiMsg ->
      state.user
        |> Api.update { onSuccess = resetLoginForm
                      , onError   = \error -> resetLoginForm } apiMsg
        |> andThen (\user -> save { state | user = user })
        |> mapCmd ApiUserMsg
        |> consumeEvents
    ApiRegisterMsg apiMsg ->
      state.registration
        |> Api.update { onSuccess = resetRegisterForm
                      , onError   = \error -> resetRegisterForm } apiMsg
        |> andThen (\registration -> save { state | registration = registration })
        |> mapCmd ApiRegisterMsg
        |> consumeEvents
    LoginFormMsg formMsg ->
      state.loginForm
        |> FormState.update { onSubmit = \form -> sendAuthRequest (LoginForm.toJson form) } formMsg
        |> andThen (\form -> save { state | loginForm = form })
        |> mapCmd LoginFormMsg
        |> consumeEvents
    RegisterFormMsg formMsg ->
      state.registerForm
        |> FormState.update { onSubmit = \form -> sendRegisterRequest (RegisterForm.toJson form) } formMsg
        |> andThen (\form -> save { state | registerForm = form })
        |> mapCmd RegisterFormMsg
        |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

loginFormView : State -> Html Msg
loginFormView { loginForm } = Html.map LoginFormMsg (FormState.view loginForm)

registerFormView : State -> Html Msg
registerFormView { registerForm } = Html.map RegisterFormMsg (FormState.view registerForm)
