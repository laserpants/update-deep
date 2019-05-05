module App.Auth exposing (..)

import App.Auth.Login.Page as LoginPage
import App.Auth.Register.Page as RegisterPage
import App.Config exposing (..)
import Update.Deep exposing (..)

type Msg
  = LoginMsg LoginPage.Msg
  | RegisterMsg RegisterPage.Msg

type alias State =
  { loginPage    : LoginPage.State
  , registerPage : RegisterPage.State }

init : Config -> Init State Msg
init config =
  let loginPage    = LoginPage.init config
      registerPage = RegisterPage.init config
   in { loginPage    = loginPage.state
      , registerPage = registerPage.state }
        |> initial
        |> initCmd LoginMsg loginPage
        |> initCmd RegisterMsg registerPage

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    LoginMsg loginMsg ->
      state.loginPage
        |> LoginPage.update loginMsg
        |> andThen (\loginPage -> save { state | loginPage = loginPage })
        |> mapCmd LoginMsg
    RegisterMsg registerMsg ->
      state.registerPage
        |> RegisterPage.update registerMsg
        |> andThen (\registerPage -> save { state | registerPage = registerPage })
        |> mapCmd RegisterMsg

subscriptions : State -> Sub Msg
subscriptions { loginPage, registerPage } =
  Sub.batch
    [ Sub.map LoginMsg (LoginPage.subscriptions loginPage)
    , Sub.map RegisterMsg (RegisterPage.subscriptions registerPage) ]
