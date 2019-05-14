module UiFormView exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Form.Error as Error exposing (..)
import Form exposing (Form)
import Form.View exposing (..)
import Html exposing (..)
import Html.Attributes as Attributes exposing (placeholder)
import Html.Events

errorToString : Error -> String
errorToString error =
  case error of
    Error.RequiredFieldIsEmpty ->
      "This field is required"
    Error.ValidationFailed description ->
      description

textareaField : TextFieldConfig msg -> Html msg
textareaField { attributes, onChange, disabled, value, error, showError } =
  let err = Maybe.withDefault "" (Maybe.map errorToString error)
   in Bootstrap.Form.group []
        [ Bootstrap.Form.label [] [ text attributes.label ]
        , Textarea.textarea
          ( List.concat
            [ if showError && Nothing /= error then [ Textarea.danger ] else []
            , if disabled then [ Textarea.disabled ] else []
            , [ Textarea.attrs [ placeholder attributes.placeholder ]
              , Textarea.onInput onChange 
              , Textarea.value value ] ] )
        , Bootstrap.Form.invalidFeedback [] [ text err ] ]

passwordField : TextFieldConfig msg -> Html msg
passwordField { attributes, onChange, disabled, value, error, showError } =
  let err = Maybe.withDefault "" (Maybe.map errorToString error)
   in Bootstrap.Form.group []
        [ Bootstrap.Form.label [] [ text attributes.label ]
        , Input.password
          ( List.concat
            [ if showError && Nothing /= error then [ Input.danger ] else []
            , [ Input.attrs [ placeholder attributes.placeholder ]
              , Input.onInput onChange 
              , Input.disabled disabled
              , Input.value value ] ] )
        , Bootstrap.Form.invalidFeedback [] [ text err ] ]

textField : TextFieldConfig msg -> Html msg
textField { attributes, onChange, disabled, value, error, showError } =
  let err = Maybe.withDefault "" (Maybe.map errorToString error)
   in Bootstrap.Form.group []
        [ Bootstrap.Form.label [] [ text attributes.label ]
        , Input.text
          ( List.concat
            [ if showError && Nothing /= error then [ Input.danger ] else []
            , [ Input.attrs [ placeholder attributes.placeholder ]
              , Input.onInput onChange 
              , Input.disabled disabled
              , Input.value value ] ] )
        , Bootstrap.Form.invalidFeedback [] [ text err ] ]

form : FormConfig msg (Html msg) -> Html msg
form { onSubmit, action, loading, state, fields } =
  let attrs = case onSubmit of
        Nothing  -> []
        Just msg -> [ Html.Events.onSubmit msg ]
   in Bootstrap.Form.form 
        attrs
        ( List.concat
          [ fields
          , [ Button.button [ Button.primary
                            , Button.disabled (onSubmit == Nothing) ]
                            [ text (if state == Loading then loading else action) ]
            ]
          ]
        )

view : ViewConfig values msg -> Form values msg -> Model values -> Html msg
view = 
  Form.View.custom 
    { htmlViewConfig 
      | textField     = textField
      , passwordField = passwordField
      , textareaField = textareaField 
      , form          = form }
