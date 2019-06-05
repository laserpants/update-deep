port module Ports exposing (..)

type alias User =
  { id : Int
  , username : String
  , name : String
  , email : String 
  }

type alias Session =
  { user : User
  }

port setSession : Session -> Cmd msg

port clearSession : () -> Cmd msg

port websocketIn : (String -> msg) -> Sub msg

port websocketOut : String -> Cmd msg
