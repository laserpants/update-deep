module Form.NewPost exposing (Fields, toJson, validate, view)

import Bulma.Form exposing (controlEmail, controlHelp, controlInputModifiers, controlLabel, controlTextArea, controlTextAreaModifiers)
import Bulma.Modifiers exposing (..)
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (Validation, andMap, field, succeed)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Json.Encode as Encode


type alias Fields =
    { title : String
    , body : String
    }


validate : Validation Never Fields
validate =
    succeed Fields
        |> andMap (field "title" validateStringNonEmpty)
        |> andMap (field "body" validateStringNonEmpty)


toJson : Fields -> Json.Value
toJson { title, body } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "body", Encode.string body )
        ]


view : Form Never Fields -> Bool -> (Form.Msg -> msg) -> Html msg
view form disabled toMsg =
    let
        info =
            fieldInfo (always "")

        title =
            form |> Form.getFieldAsString "title" |> info controlInputModifiers

        body =
            form |> Form.getFieldAsString "body" |> info controlTextAreaModifiers
    in
    [ fieldset [ Html.Attributes.disabled disabled ]
        [ Bulma.Form.field []
            [ Bulma.Form.controlLabel [] [ text "Title" ]
            , Bulma.Form.controlInput title.modifiers
                []
                [ placeholder "Title"
                , onFocus (Form.Focus title.path)
                , onBlur (Form.Blur title.path)
                , onInput (String >> Form.Input title.path Form.Text)
                , value (Maybe.withDefault "" title.value)
                ]
                []
            , Bulma.Form.controlHelp Danger [] [ Html.text title.errorMessage ]
            ]
        , Bulma.Form.field []
            [ Bulma.Form.controlLabel [] [ text "Body" ]
            , Bulma.Form.controlTextArea body.modifiers
                []
                [ placeholder "Body"
                , onFocus (Form.Focus body.path)
                , onBlur (Form.Blur body.path)
                , onInput (String >> Form.Input body.path Form.Text)
                , value (Maybe.withDefault "" body.value)
                ]
                []
            , Bulma.Form.controlHelp Danger [] [ Html.text body.errorMessage ]
            ]
        , Bulma.Form.field []
            [ div [ class "control" ]
                [ button [ type_ "submit", class "button is-primary" ]
                    [ text
                        (if disabled then
                            "Please wait"

                         else
                            "Publish"
                        )
                    ]
                ]
            ]
        ]
    ]
        |> Html.form [ onSubmit Form.Submit ]
        |> Html.map toMsg
