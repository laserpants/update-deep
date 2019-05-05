module Data.Comment exposing (..)

import Json.Decode as Json exposing (field)

type alias Comment =
  { id    : Int
  , email : String
  , body  : String }

decoder : Json.Decoder Comment
decoder =
  Json.map3 Comment
    (field "id"    Json.int)
    (field "email" Json.string)
    (field "body"  Json.string)
