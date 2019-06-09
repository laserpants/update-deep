module Page.Register exposing (..)

import Bulma.Modifiers exposing (..)
import Dict exposing (Dict)
import Form exposing (Form)
import Update.Deep.Form 
import Update.Deep.Api as Api
import Form.Register
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

--

type alias WebSocketUsernameAvailableResponsePayload =
  { username : String
  , available : Bool 
  }

webSocketUsernameAvailableResponseDecoder : Json.Decoder WebSocketUsernameAvailableResponsePayload
webSocketUsernameAvailableResponseDecoder =
  Json.map2 WebSocketUsernameAvailableResponsePayload
    (Json.field "username" Json.string)
    (Json.field "available" Json.bool)

type WebSocketMessage
  = WebSocketUsernameAvailableResponse WebSocketUsernameAvailableResponsePayload

websocketMessageDecoder : Json.Decoder WebSocketMessage
websocketMessageDecoder =
  let payloadDecoder type_ =
        case type_ of
          "username_available_response" ->
            Json.map WebSocketUsernameAvailableResponse webSocketUsernameAvailableResponseDecoder
          _ ->
            Json.fail "Unrecognized message type"
   in Json.field "type" Json.string |> Json.andThen payloadDecoder

--

type Msg 
  = RegisterPageApiMsg (Api.Msg User)
  | RegisterFormMsg Form.Msg
  | RegisterPageWebsocketMsg String

type UsernameStatus
  = UsernameBlank
  | UsernameAvailable Bool
  | Unknown

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
        |> andMap (save UsernameBlank)
        |> mapCmd toMsg

websocketUsernameAvailableQuery : String -> Json.Value
websocketUsernameAvailableQuery username =
  Encode.object
    [ ( "type"  , Encode.string "username_available_query" )
    , ( "username" , Encode.string username ) 
    ]

checkIfUsernameAvailable : String -> State -> Update State msg a
checkIfUsernameAvailable username ({ usernames } as state) = 
  if String.isEmpty username
      then 
        state
          |> setUsernameStatus UsernameBlank
      else 
        case Dict.get username usernames of
          Just isAvailable ->
            state
              |> setUsernameStatus (UsernameAvailable isAvailable)
          Nothing ->
            state
              |> setUsernameStatus Unknown
              |> andAddCmd (Ports.websocketOut (Encode.encode 0 (websocketUsernameAvailableQuery username)))

usernameFieldSpy : Form.Msg -> State -> Update State msg a
usernameFieldSpy formMsg =
  case formMsg of
    Form.Input "username" Form.Text (String username) ->
      checkIfUsernameAvailable username 
    _ ->
      save 

handleSubmit : (Msg -> msg) -> Form.Register.Fields -> State -> Update State msg a
handleSubmit toMsg form =
  let json = form |> Form.Register.toJson |> Http.jsonBody 
   in inApi (Api.sendRequest "" (Just json) (toMsg << RegisterPageApiMsg))

update : Msg -> (Msg -> msg) -> State -> Update State msg a
update msg toMsg = 
  case msg of
    RegisterPageApiMsg apiMsg ->
      inApi (Api.update { onSuccess = always save, onError = always save } apiMsg (toMsg << RegisterPageApiMsg))
    RegisterFormMsg formMsg ->
      inForm (Update.Deep.Form.update { onSubmit = handleSubmit toMsg } formMsg)
        >> andThen (usernameFieldSpy formMsg)
    RegisterPageWebsocketMsg websocketMsg ->
      case Json.decodeString websocketMessageDecoder websocketMsg of
        Ok (WebSocketUsernameAvailableResponse { username, available }) ->
          unwrap .formModel (\model -> 
            let 
                usernameField = Form.getFieldAsString "username" model.form
             in 
                saveUsernameStatus username available
                  >> andThen (checkIfUsernameAvailable <| Maybe.withDefault "" usernameField.value))
        _ ->
          save 

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Ports.websocketIn (toMsg << RegisterPageWebsocketMsg)

formView : Update.Deep.Form.Model Form.Register.Custom.Error Form.Register.Fields -> UsernameStatus -> (Form.Msg -> msg) -> Html msg
formView { form, disabled } usernameStatus toMsg =

  let 
      info = fieldInfo Form.Register.Custom.errorToString controlInputModifiers 

      name                 = form |> Form.getFieldAsString "name"                 |> info
      email                = form |> Form.getFieldAsString "email"                |> info
      phoneNumber          = form |> Form.getFieldAsString "phoneNumber"          |> info
      password             = form |> Form.getFieldAsString "password"             |> info
      passwordConfirmation = form |> Form.getFieldAsString "passwordConfirmation" |> info
      agreeWithTerms       = form |> Form.getFieldAsBool   "agreeWithTerms"       |> info

      availableIcon = ( Small, [], i [ class "fa fa-check has-text-success" ] [] )
      unavailableIcon = ( Small, [], i [ class "fa fa-times has-text-danger" ] [] )

      username =

        let 
            info_ = form |> Form.getFieldAsString "username" |> info
         in 
            case usernameStatus of
              UsernameAvailable True ->
                { info_ | modifiers = { controlInputModifiers | color = Success, iconRight = Just availableIcon } }
              UsernameAvailable False ->
                { info_ | modifiers = { controlInputModifiers | color = Danger, iconRight = Just unavailableIcon }
                        , errorMessage = "This username is not available" }
              _ ->
                info_
   in
      [ fieldset [ Html.Attributes.disabled disabled ]
        [ Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Name" ] 
          , Bulma.Form.controlInput name.modifiers [] 
            [ placeholder "Name"
            , onFocus (Form.Focus name.path)
            , onBlur (Form.Blur name.path)
            , onInput (String >> Form.Input name.path Form.Text)
            , value (Maybe.withDefault "" name.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text name.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Email" ] 
          , Bulma.Form.controlEmail email.modifiers [] 
            [ placeholder "Email"
            , onFocus (Form.Focus email.path)
            , onBlur (Form.Blur email.path)
            , onInput (String >> Form.Input email.path Form.Text)
            , value (Maybe.withDefault "" email.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text email.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Username" ] 
          , Bulma.Form.controlInput username.modifiers
              (if Unknown == usernameStatus then [ class "is-loading" ] else [])
            [ placeholder "Username"
            , onFocus (Form.Focus username.path)
            , onBlur (Form.Blur username.path)
            , onInput (String >> Form.Input username.path Form.Text)
            , value (Maybe.withDefault "" username.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text username.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Phone number" ] 
          , Bulma.Form.controlPhone phoneNumber.modifiers [] 
            [ placeholder "Phone number"
            , onFocus (Form.Focus phoneNumber.path)
            , onBlur (Form.Blur phoneNumber.path)
            , onInput (String >> Form.Input phoneNumber.path Form.Text)
            , value (Maybe.withDefault "" phoneNumber.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text phoneNumber.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Password" ] 
          , Bulma.Form.controlPassword password.modifiers [] 
            [ placeholder "Password"
            , onFocus (Form.Focus password.path)
            , onBlur (Form.Blur password.path)
            , onInput (String >> Form.Input password.path Form.Text)
            , value (Maybe.withDefault "" password.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text password.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Password confirmation" ] 
          , Bulma.Form.controlPassword passwordConfirmation.modifiers [] 
            [ placeholder "Password confirmation"
            , onFocus (Form.Focus passwordConfirmation.path)
            , onBlur (Form.Blur passwordConfirmation.path)
            , onInput (String >> Form.Input passwordConfirmation.path Form.Text)
            , value (Maybe.withDefault "" passwordConfirmation.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text passwordConfirmation.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlCheckBox False [] [] 
            [ onFocus (Form.Focus agreeWithTerms.path)
            , onBlur (Form.Blur agreeWithTerms.path)
            , onCheck (Bool >> Form.Input agreeWithTerms.path Form.Checkbox)
            , checked (Maybe.withDefault False agreeWithTerms.value)
            ] [ text "I agree with terms and conditions" ]
          , Bulma.Form.controlHelp Danger [] [ Html.text agreeWithTerms.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ div [ class "control" ] 
            [ button [ type_ "submit", class "button is-primary" ] 
              [ text (if disabled then "Please wait" else "Send") ] 
            ]
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

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
                resourceErrorView api.resource
              _ ->
                formView formModel usernameStatus (toMsg << RegisterFormMsg)
          ]
        ]
      ]
    ]
