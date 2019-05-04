module App.Posts.Form exposing (..)

import Form exposing (Form)
import FormState exposing (..)
import Json.Encode as Encode exposing (Value, object)

type alias Form =
  { title : String
  , body  : String }

fields : Fields Form
fields =

  let titleField =
        Form.textField
          { parser = Ok
          , value  = .title
          , update = \value values -> { values | title = value }
          , attributes =
            { label       = "Title"
            , placeholder = "Title" } }

      bodyField =
        Form.textareaField
          { parser = Ok
          , value  = .body
          , update = \value values -> { values | body = value }
          , attributes =
            { label       = "Body"
            , placeholder = "Body" } }

   in Form.succeed Form
    |> Form.append titleField
    |> Form.append bodyField
    |> Form.map Submit

toJson : Form -> Value
toJson { title, body } =
  object [ ( "title" , Encode.string title )
         , ( "body"  , Encode.string body ) ]
