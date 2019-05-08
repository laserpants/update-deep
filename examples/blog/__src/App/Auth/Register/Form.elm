module App.Auth.Register.Form exposing (..)

import Form exposing (Form)
import Json.Encode as Encode exposing (Value, object)
import Update.Form exposing (Fields, Msg(..))

type alias RegisterForm =
  { login    : String
  , password : String }

fields : Fields RegisterForm
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

   in Form.succeed RegisterForm
    |> Form.append loginField
    |> Form.append passwordField
    |> Form.map Submit

toJson : RegisterForm -> Value
toJson { login, password } =
  object [ ( "login"    , Encode.string login )
         , ( "password" , Encode.string password ) ]
