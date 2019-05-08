module App.Auth.Register exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Auth.Register.Form as RegisterForm exposing (RegisterForm)
import App.Config exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Update.Deep exposing (..)
import Update.Form as Form

type Msg
  = ApiMsg (Api.Msg { status : String })
  | FormMsg (Form.Msg RegisterForm)

type alias State =
  { response : Api { status : String }
  , form     : Form.State RegisterForm }

responseDecoder : Json.Decoder { status : String }
responseDecoder =
  field "status" Json.string |> Json.map (\status -> { status = status })

init : Config -> Init State Msg
init { flags } =
  let response = Api.init { endpoint = flags.api ++ "/auth/register"
                          , method   = HttpPost
                          , decoder  = responseDecoder }
      form = Form.init RegisterForm.fields
                          { login    = ""
                          , password = "" }
   in { response = response.state
      , form     = form.state }
        |> initial
        |> initCmd ApiMsg response
        |> initCmd FormMsg form

onSubmit : RegisterForm -> State -> Update State Msg a
onSubmit form =
  RegisterForm.toJson form
    |> Api.jsonRequest
    |> ApiMsg
    |> update

update : Msg -> State -> Update State Msg a
update msg state =
  let resetForm = update (FormMsg Form.Reset)
      default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.response
            |> Api.update { default | onSuccess = always resetForm } apiMsg
            |> andThen (\response -> save { state | response = response })
            |> mapCmd ApiMsg
            |> consumeEvents
        FormMsg formMsg ->
          state.form
            |> Form.update { onSubmit = onSubmit } formMsg
            |> andThen (\form -> save { state | form = form })
            |> mapCmd FormMsg
            |> consumeEvents

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

view : State -> Html Msg
view { form } = Html.map FormMsg (Form.view form)
