module Page.Register exposing (..)

import Bulma.Modifiers exposing (..)
import Dict exposing (Dict)
import Form exposing (Form)
import Update.Deep.Form 
import Update.Deep.Api as Api
import Form.Register exposing (UsernameStatus(..))
import Form.Register.Custom
import Http 
import Form.Field as Field exposing (Field, FieldValue(..))
import Data.User as User exposing (User)
import Json.Decode as Json
import Update.Deep.Api as Api exposing (Resource(..))
import Update.Deep exposing (..)
import Bulma.Components exposing (..)
import Json.Encode as Encode
import Ports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bulma.Form exposing (controlInputModifiers)
import Helpers exposing (..)

type alias WebSocketIsAvailableResponsePayload =
  { username : String
  , available : Bool 
  }

webSocketIsAvailableResponseDecoder : Json.Decoder WebSocketIsAvailableResponsePayload
webSocketIsAvailableResponseDecoder =
  Json.map2 WebSocketIsAvailableResponsePayload
    (Json.field "username" Json.string)
    (Json.field "available" Json.bool)

type WebSocketMessage
  = WebSocketIsAvailableResponse WebSocketIsAvailableResponsePayload

websocketMessageDecoder : Json.Decoder WebSocketMessage
websocketMessageDecoder =
  let payloadDecoder type_ =
        case type_ of
          "username_available_response" ->
            Json.map WebSocketIsAvailableResponse webSocketIsAvailableResponseDecoder
          _ ->
            Json.fail "Unrecognized message type"
   in Json.field "type" Json.string |> Json.andThen payloadDecoder

--

type Msg 
  = ApiMsg (Api.Msg User)
  | FormMsg Form.Msg
  | WebsocketMsg String

type alias State =
  { api : Api.Model User 
  , formModel : Update.Deep.Form.Model Form.Register.Custom.Error Form.Register.Fields
  , usernames : Dict String Bool 
  , usernameStatus : UsernameStatus
  }

saveUsernameStatus : String -> Bool -> State -> Update State msg a
saveUsernameStatus username available state = save { state | usernames = Dict.insert username available state.usernames }

inApi : In State (Api.Model User) msg a
inApi =
    inState { get = .api, set = \state api -> { state | api = api } }

inForm : In State (Update.Deep.Form.Model Form.Register.Custom.Error Form.Register.Fields) msg a
inForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

setUsernameStatus : UsernameStatus -> State -> Update State msg a
setUsernameStatus status state = save { state | usernameStatus = status }

init : (Msg -> msg) -> Update State msg a
init toMsg = 

  let 
      api = Api.init { endpoint = "/auth/register"
                     , method   = Api.HttpPost
                     , decoder  = Json.field "user" User.decoder }
   in 
      save State
        |> andMap api
        |> andMap (Update.Deep.Form.init [] Form.Register.validate)
        |> andMap (save Dict.empty)
        |> andMap (save Blank)
        |> mapCmd toMsg

websocketIsAvailableQuery : String -> Json.Value
websocketIsAvailableQuery username =
  Encode.object
    [ ( "type"  , Encode.string "username_available_query" )
    , ( "username" , Encode.string username ) 
    ]

checkIfIsAvailable : String -> State -> Update State msg a
checkIfIsAvailable username ({ usernames } as state) = 
  if String.isEmpty username
      then 
        state
          |> setUsernameStatus Blank
      else 
        case Dict.get username usernames of
          Just isAvailable ->
            state
              |> setUsernameStatus (IsAvailable isAvailable)
          Nothing ->
            state
              |> setUsernameStatus Unknown
              |> andAddCmd (Ports.websocketOut (Encode.encode 0 (websocketIsAvailableQuery username)))

usernameFieldSpy : Form.Msg -> State -> Update State msg a
usernameFieldSpy formMsg =
  case formMsg of
    Form.Input "username" Form.Text (String username) ->
      checkIfIsAvailable username 
    _ ->
      save 

handleSubmit : (Msg -> msg) -> Form.Register.Fields -> State -> Update State msg a
handleSubmit toMsg form =
  let json = form |> Form.Register.toJson |> Http.jsonBody 
   in inApi (Api.sendRequest "" (Just json) (toMsg << ApiMsg))

update : Msg -> (Msg -> msg) -> State -> Update State msg a
update msg toMsg = 
  case msg of
    ApiMsg apiMsg ->
      inApi (Api.update { onSuccess = always save, onError = always save } apiMsg (toMsg << ApiMsg))
    FormMsg formMsg ->
      inForm (Update.Deep.Form.update { onSubmit = handleSubmit toMsg } formMsg)
        >> andThen (usernameFieldSpy formMsg)
    WebsocketMsg websocketMsg ->
      case Json.decodeString websocketMessageDecoder websocketMsg of
        Ok (WebSocketIsAvailableResponse { username, available }) ->
          unwrap .formModel (\model -> 
            let 
                usernameField = Form.getFieldAsString "username" model.form
             in 
                saveUsernameStatus username available
                  >> andThen (checkIfIsAvailable <| Maybe.withDefault "" usernameField.value))
        _ ->
          save 

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Ports.websocketIn (toMsg << WebsocketMsg)

view : State -> (Msg -> msg) -> Html msg
view { api, formModel, usernameStatus } toMsg = 
  div [ class "columns is-centered", style "margin" "1.5em" ] 
    [ div [ class "column is-half" ] 
      [ card [] 
        [ cardContent []
          [ h3 [ class "title is-3" ] [ text "Register" ] 
          , case api.resource of
              Available response ->
                div [] 
                  [ text "Thanks for registering! Now head over to the "
                  , a [ href "/login" ] [ text "log in" ]
                  , text " page and see how it goes." ]
              Api.Error error ->
                apiResourceErrorMessage api.resource
              _ ->
                Form.Register.view formModel.form formModel.disabled usernameStatus (toMsg << FormMsg)
          ]
        ]
      ]
    ]
