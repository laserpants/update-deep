module Data.Post exposing (..)

import Json.Decode as Json exposing (field)

type alias Post =
  { id    : Int
  , title : String
  , body  : String }

decoder : Json.Decoder Post
decoder =
  Json.map3 Post
    (field "id"    Json.int)
    (field "title" Json.string)
    (field "body"  Json.string)
