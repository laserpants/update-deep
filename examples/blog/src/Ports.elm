port module Ports exposing (..)

type alias User =
  { username : String
  , name : String
  , email : String 
  }

type alias Session =
  { user : User
  }

port setSession : Session -> Cmd msg

port clearSession : () -> Cmd msg
