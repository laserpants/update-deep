module App.Auth exposing (..)

import App.Auth.Login as Login
import App.Auth.Register as Register
import App.Config exposing (..)
import Update.Deep exposing (..)

type Msg
  = LoginMsg Login.Msg
  | RegisterMsg Register.Msg

type alias State =
  { login    : Login.State
  , register : Register.State }

init : Config -> Init State Msg
init config =
  let login    = Login.init config
      register = Register.init config
   in { login    = login.state
      , register = register.state }
        |> initial
        |> initCmd LoginMsg login
        |> initCmd RegisterMsg register

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    LoginMsg loginMsg ->
      state.login
        |> Login.update loginMsg
        |> andThen (\page -> save { state | login = page })
        |> mapCmd LoginMsg
    RegisterMsg registerMsg ->
      state.register
        |> Register.update registerMsg
        |> andThen (\page -> save { state | register = page })
        |> mapCmd RegisterMsg

subscriptions : State -> Sub Msg
subscriptions { login, register } =
  Sub.batch
    [ Sub.map LoginMsg (Login.subscriptions login)
    , Sub.map RegisterMsg (Register.subscriptions register) ]
