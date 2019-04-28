# Update Pointfree

```elm
module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Update.Pointfree exposing (Update, Init, copy, initial)
import Update.Pointfree.Browser as Update

type Msg
  = SomeMsg
  | OtherMsg
  
type alias Model =
  { 
  -- ... 
  }

view : Model -> Document Msg
view model =
  { title = ""
  , body  = [ div [] [] ] }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
```

then

```elm
init : () -> Init Model Msg
init _ = initial {}

update : Msg -> Update Model Msg ()
update msg = copy

main : Program () Model Msg
main =
  Update.document
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
```

