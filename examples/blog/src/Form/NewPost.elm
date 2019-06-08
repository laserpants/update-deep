module Form.NewPost exposing (..)

import Form.Validate as Validate exposing (Validation, succeed, field)
import Json.Decode as Json
import Json.Encode as Encode
import Helpers exposing (..)

type alias Fields =
  { title : String
  , body : String 
  }

validate : Validation Never Fields
validate =
  succeed Fields
    |> Validate.andMap (field "title" validateStringNonEmpty)
    |> Validate.andMap (field "body" validateStringNonEmpty)

toJson : Fields -> Json.Value
toJson { title, body } = 
  Encode.object 
    [ ( "title" , Encode.string title )
    , ( "body" , Encode.string body )
    ]
