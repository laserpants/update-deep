# Update Pointfree

## How to use this library

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
--  ... 
  }

type alias Flags = ()

view : Model -> Document Msg
view model =
  { title = ""
  , body  = [ text "" ] }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- The above lines are just business as usual. 

init : Flags -> Init Model Msg             -- [1]
init _ = initial {}                     -- [2]

update : Msg -> Update Model Msg ()     -- [3]
update msg = copy                       -- [4]

main : Program Flags Model Msg
main =
  Update.document                       -- [5]
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
```

