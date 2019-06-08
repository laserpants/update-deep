module Form.Comment exposing (..)

import Json.Encode as Encode
import Form.Validate as Validate exposing (Validation, succeed, field)
import Helpers exposing (..)
import Json.Decode as Json

type alias Fields =
  { email : String
  , body : String 
  }

validate : Validation Never Fields
validate =
  succeed Fields
    |> Validate.andMap (field "email" validateEmail)
    |> Validate.andMap (field "body" validateStringNonEmpty)

toJson : Int -> Fields -> Json.Value
toJson postId { email, body } = 
  Encode.object 
    [ ( "postId" , Encode.int postId )
    , ( "email" , Encode.string email )
    , ( "body" , Encode.string body )
    ]
