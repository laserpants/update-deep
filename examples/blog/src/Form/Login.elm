module Form.Login exposing (..)

import Form.Validate as Validate exposing (Validation, succeed, field)
import Json.Encode as Encode
import Json.Decode as Json
import Helpers exposing (..)

type alias Fields =
  { username : String
  , password : String 
  , rememberMe : Bool
  }

validate : Validation Never Fields
validate =
  succeed Fields
    |> Validate.andMap (field "username" validateStringNonEmpty)
    |> Validate.andMap (field "password" validateStringNonEmpty)
    |> Validate.andMap (field "rememberMe" Validate.bool)

toJson : Fields -> Json.Value
toJson { username, password, rememberMe } = 
  Encode.object 
    [ ( "username" , Encode.string username )
    , ( "password" , Encode.string password )
    , ( "rememberMe" , Encode.bool rememberMe )
    ]
