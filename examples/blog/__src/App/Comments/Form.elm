module App.Comments.Form exposing (..)

import Form exposing (Form)
import Json.Encode as Encode exposing (Value, object)
import Update.Form exposing (Fields, Msg(..))

type alias Form =
  { email   : String
  , comment : String }

fields : Fields Form
fields =

  let emailField =
        Form.textField
          { parser = Ok
          , value  = .email
          , update = \value values -> { values | email = value }
          , attributes =
            { label       = "Email"
            , placeholder = "Email" } }

      commentField =
        Form.textareaField
          { parser = Ok
          , value  = .comment
          , update = \value values -> { values | comment = value }
          , attributes =
            { label       = "Comment"
            , placeholder = "Comment" } }

   in Form.succeed Form
    |> Form.append emailField
    |> Form.append commentField
    |> Form.map Submit

toJson : Form -> Value
toJson { email, comment } =
  object [ ( "email"   , Encode.string email )
         , ( "comment" , Encode.string comment ) ]
