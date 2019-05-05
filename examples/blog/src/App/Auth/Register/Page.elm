module App.Auth.Register.Page exposing (..)

import Api exposing (Api, HttpMethod(..))
import App.Auth.Register.Form as RegisterForm exposing (RegisterForm)
import App.Config exposing (..)
import FormState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Update.Deep exposing (..)

type Msg
  = ApiMsg (Api.Msg { status : String })
  | FormMsg (FormState.Msg RegisterForm)

type alias State =
  { response : Api { status : String }
  , form     : FormState RegisterForm }

responseDecoder : Json.Decoder { status : String }
responseDecoder =
  field "status" Json.string |> Json.map (\status -> { status = status })

init : Config -> Init State Msg
init { flags } =
  let response = Api.init { endpoint = flags.api ++ "/auth/register"
                          , method   = HttpPost
                          , decoder  = responseDecoder }
      form = FormState.init RegisterForm.fields
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
  let resetForm = update (FormMsg FormState.Reset)
      default = Api.defaultHandlers
   in case msg of
        ApiMsg apiMsg ->
          state.response
            |> Api.update { default | onSuccess = resetForm } apiMsg
            |> andThen (\response -> save { state | response = response })
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
view { form } = Html.map FormMsg (FormState.view form)
