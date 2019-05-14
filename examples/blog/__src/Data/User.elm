module Data.User exposing (..)

import Json.Decode as Json exposing (field)

type alias User =
  { id    : Int
  , email : String
  , login : String
  , name  : String }

decoder : Json.Decoder User
decoder =
  Json.map4 User
    (field "id"    Json.int)
    (field "email" Json.string)
    (field "login" Json.string)
    (field "name"  Json.string)
