module Page.NewPost exposing (..)

import Data.Post as Post exposing (Post)
import Json.Decode as Json
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.NewPost
import Update.Deep exposing (..)
import Update.Deep.Api as Api
import Update.Deep.Form 
import Update.Deep.Form as Form
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Form exposing (controlInputModifiers, controlTextAreaModifiers)

type Msg 
  = NewPostPageApiMsg (Api.Msg Post)
  | NewPostFormMsg Form.Msg

type alias State =
  { api : Api.Model Post
  , formModel : Form.Model Never Form.NewPost.Fields }

inApi : In State (Api.Model Post) msg a
inApi =
    inState { get = .api, set = \state api -> { state | api = api } }

inForm : In State (Form.Model Never Form.NewPost.Fields) msg a
inForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

init : (Msg -> msg) -> Update State msg a
init toMsg = 
  let
      api = Api.init { endpoint = "/posts"
                     , method   = Api.HttpPost
                     , decoder  = Json.field "post" Post.decoder }
   in 
      save State
        |> andMap api
        |> andMap (Form.init [] Form.NewPost.validate)
        |> mapCmd toMsg

handleSubmit : (Msg -> msg) -> Form.NewPost.Fields -> State -> Update State msg a
handleSubmit toMsg form = 
  let json = form |> Form.NewPost.toJson |> Http.jsonBody 
   in inApi (Api.sendRequest "" (Just json) (toMsg << NewPostPageApiMsg))

update : { onPostAdded : Post -> a } -> Msg -> (Msg -> msg) -> State -> Update State msg a
update { onPostAdded } msg toMsg = 
  case msg of
    NewPostPageApiMsg apiMsg ->
      inApi (Api.update { onSuccess = invokeHandler << onPostAdded, onError = always save } apiMsg (toMsg << NewPostPageApiMsg))
    NewPostFormMsg formMsg ->
      inForm (Update.Deep.Form.update { onSubmit = handleSubmit toMsg } formMsg)

subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg = Sub.none

formView : Form.Model Never Form.NewPost.Fields -> (Form.Msg -> msg) -> Html msg
formView { form, disabled } toMsg =

  let 
      info = fieldInfo (always "")

      title = form |> Form.getFieldAsString "title" |> info controlInputModifiers
      body  = form |> Form.getFieldAsString "body"  |> info controlTextAreaModifiers
   in
      [ fieldset [ Html.Attributes.disabled disabled ]
        [ Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Title" ] 
          , Bulma.Form.controlInput title.modifiers [] 
            [ placeholder "Title"
            , onFocus (Form.Focus title.path)
            , onBlur (Form.Blur title.path)
            , onInput (String >> Form.Input title.path Form.Text)
            , value (Maybe.withDefault "" title.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text title.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ Bulma.Form.controlLabel [] [ text "Body" ] 
          , Bulma.Form.controlTextArea body.modifiers [] 
            [ placeholder "Body"
            , onFocus (Form.Focus body.path)
            , onBlur (Form.Blur body.path)
            , onInput (String >> Form.Input body.path Form.Text)
            , value (Maybe.withDefault "" body.value)
            ] [] 
          , Bulma.Form.controlHelp Danger [] [ Html.text body.errorMessage ]
          ]
        , Bulma.Form.field [] 
          [ div [ class "control" ] 
            [ button [ type_ "submit", class "button is-primary" ] 
              [ text (if disabled then "Please wait" else "Publish") ] 
            ]
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

view : State -> (Msg -> msg) -> Html msg
view { api, formModel } toMsg = 
  div [ class "columns is-centered", style "margin" "1.5em" ] 
    [ div [ class "column is-two-thirds" ] 
      [ h3 [ class "title is-3" ] [ text "New post" ] 
      , resourceErrorView api.resource
      , formView formModel (toMsg << NewPostFormMsg) ]
    ]
