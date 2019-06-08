module Data.Post exposing (..)

import Data.Comment as Comment exposing (Comment)
import Json.Decode as Json exposing (field, int, string)

type alias Post =
  { id    : Int
  , title : String
  , body  : String 
  , comments : List Comment
  }

decoder : Json.Decoder Post
decoder =
  Json.map4 Post
    (field "id"    int)
    (field "title" string)
    (field "body"  string)
    (field "comments" (Json.list Comment.decoder))
