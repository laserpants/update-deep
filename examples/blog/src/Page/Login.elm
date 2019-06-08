module Page.Login exposing (..)

import Http 
import Form.Field as Field exposing (Field, FieldValue(..))
import Update.Deep.Form as Form
import Update.Deep.Form 
import Update.Deep.Api as Api
import Form exposing (Form)
import Form.Login
import Data.Session as Session exposing (Session)
import Json.Decode as Json
import Update.Deep exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers exposing (..)
import Bulma.Components exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Form exposing (controlInputModifiers)

type Msg 
  = LoginFormMsg Form.Msg
  | LoginPageApiMsg (Api.Msg Session)

type alias State =
  { api : Api.Model Session
  , formModel : Form.Model Never Form.Login.Fields
  }

inApi : In State (Api.Model Session) msg a
inApi =
    inState { get = .api, set = \state api -> { state | api = api } }

inForm : In State (Form.Model Never Form.Login.Fields) msg a
inForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

init : (Msg -> msg) -> Update State msg a
init toMsg = 

  let 
      api = Api.init { endpoint = "/auth/login"
                     , method   = Api.HttpPost
                     , decoder  = Json.field "session" Session.decoder }
   in 
      save State
        |> andMap api
        |> andMap (Form.init [] Form.Login.validate)
        |> mapCmd toMsg

handleSubmit : (Msg -> msg) -> Form.Login.Fields -> State -> Update State msg a
handleSubmit toMsg form = 
  let json = form |> Form.Login.toJson |> Http.jsonBody 
   in inApi (Api.sendRequest "" (Just json) (toMsg << LoginPageApiMsg))

update : { onAuthResponse : Maybe Session -> a } -> Msg -> (Msg -> msg) -> State -> Update State msg a
update { onAuthResponse } msg toMsg =

  let 
      handleApiResponse maybeSession = 
        inForm (Form.reset []) 
          >> andInvokeHandler (onAuthResponse maybeSession)
   in 
      case msg of
        LoginPageApiMsg apiMsg -> 
          inApi (Api.update { onSuccess = handleApiResponse << Just, onError = handleApiResponse Nothing |> always } apiMsg (toMsg << LoginPageApiMsg))
        LoginFormMsg formMsg ->
          inForm (Update.Deep.Form.update { onSubmit = handleSubmit toMsg } formMsg)

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Sub.none

loginPageFormView : Form.Model Never Form.Login.Fields -> (Form.Msg -> msg) -> Html msg
loginPageFormView { form, disabled } toMsg =

  let 
      info = fieldInfo (always "")

      usernameIcon = Just ( Small, [], i [ class "fa fa-user" ] [] )
      passwordIcon = Just ( Small, [], i [ class "fa fa-lock" ] [] )

      username   = form |> Form.getFieldAsString "username"   |> info { controlInputModifiers | iconLeft = usernameIcon }
      password   = form |> Form.getFieldAsString "password"   |> info { controlInputModifiers | iconLeft = passwordIcon }
      rememberMe = form |> Form.getFieldAsBool   "rememberMe" |> info controlInputModifiers 
   in
      [ fieldset [ Html.Attributes.disabled disabled ]
        [ Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Username" ] 
          , Bulma.Form.controlInput username.modifiers [] 
            [ placeholder "Username"
            , onFocus (Form.Focus username.path)
            , onBlur (Form.Blur username.path)
            , onInput (String >> Form.Input username.path Form.Text)
            , value (Maybe.withDefault "" username.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text username.errorMessage ]
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
          [ Bulma.Form.controlCheckBox False [] [] 
            [ onFocus (Form.Focus rememberMe.path)
            , onBlur (Form.Blur rememberMe.path)
            , onCheck (Bool >> Form.Input rememberMe.path Form.Checkbox)
            , checked (Maybe.withDefault False rememberMe.value)
            ] [ text "Remember me" ]
          , Bulma.Form.controlHelp Danger [] [ Html.text rememberMe.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ div [ class "control" ] 
            [ button [ type_ "submit", class "button is-primary" ] 
              [ text (if disabled then "Please wait" else "Log in") ] 
            ]
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

view : State -> (Msg -> msg) -> Html msg
view { api, formModel } toMsg = 
  div [ class "columns is-centered is-mobile", style "margin" "6em 0" ] 
    [ div [ class "column is-narrow" ] 
      [ card [] 
        [ cardContent []
          [ h3 [ class "title is-3" ] [ text "Log in" ] 
          , message { messageModifiers | color = Info } 
            [ style "max-width" "360px" ] 
            [ messageBody [] 
              [ text "This is a demo. Log in with username 'test' and password 'test'." ] 
            ]
          , resourceErrorView api.resource
          , loginPageFormView formModel (toMsg << LoginFormMsg) 
          ]
        ]
      ]
    ]
