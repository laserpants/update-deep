module Data.User exposing (..)

import Json.Decode as Json exposing (field)

type alias User =
  { email : String
  , login : String
  , name  : String }

decoder : Json.Decoder User
decoder =
  Json.map3 User
    (field "email" Json.string)
    (field "login" Json.string)
    (field "name"  Json.string)
