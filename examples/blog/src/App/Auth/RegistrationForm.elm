module App.Auth.RegistrationForm exposing (..)

import Form exposing (Form)
import FormState exposing (..)
import Json.Encode as Encode exposing (Value, object)

type alias RegistrationForm =
  { login    : String
  , password : String }

fields : Fields RegistrationForm
fields =

  let loginField =
        Form.textField
          { parser = Ok
          , value  = .login
          , update = \value values -> { values | login = value }
          , attributes =
            { label       = "Email"
            , placeholder = "Email" } }

      passwordField =
        Form.passwordField
          { parser = Ok
          , value  = .password
          , update = \value values -> { values | password = value }
          , attributes =
            { label       = "Password"
            , placeholder = "Your password" } }

   in Form.succeed RegistrationForm
    |> Form.append loginField
    |> Form.append passwordField
    |> Form.map Submit

toJson : RegistrationForm -> Value
toJson { login, password } =
  object [ ( "login"    , Encode.string login )
         , ( "password" , Encode.string password ) ]
