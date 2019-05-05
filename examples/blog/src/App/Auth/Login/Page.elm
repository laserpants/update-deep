module App.Auth.Login.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Auth.Login.Form as LoginForm exposing (LoginForm)
import App.Config exposing (..)
import Data.User as User exposing (User)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg User)
  | FormMsg (FormState.Msg LoginForm)

type alias State =
  { user : Api User
  , form : FormState LoginForm }

init : Config -> Init State Msg
init { flags } = 
  let user = Api.init { endpoint = flags.api ++ "/auth/login"
                      , method   = HttpPost
                      , decoder  = Json.field "user" User.decoder }
      form = FormState.init LoginForm.fields
                      { login    = ""
                      , password = "" }
   in { user = user.state
      , form = form.state }
        |> initial
        |> initCmd ApiMsg user
        |> initCmd FormMsg form

onSubmit : LoginForm -> State -> Update State Msg a
onSubmit form =
  LoginForm.toJson form
    |> Api.jsonRequest
    |> ApiMsg
    |> update

update : Msg -> State -> Update State Msg a
update msg state =
  let resetForm = update (FormMsg FormState.Reset)
      default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.user
            |> Api.update { default | onSuccess = resetForm } apiMsg
            |> andThen (\user -> save { state | user = user })
            |> mapCmd ApiMsg
            |> consumeEvents
        FormMsg formMsg ->
          state.form
            |> FormState.update { onSubmit = onSubmit } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view state = div [] []
