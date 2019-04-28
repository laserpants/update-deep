# Update Pointfree

TODO

## How to use this library

```elm
module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Update.Pointfree exposing (Update, Init, copy, initial)
import Update.Pointfree.Browser as Update

-- You create your Msg type, Model, views, and subscriptions just as you'd normally do:

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

-- The above is just business as usual. The only real difference is in the init, update, and 
-- main calls below.

init : Flags -> Init Model Msg          -- [2]
init _ = initial {}                     -- [3]

update : Msg -> Update Model Msg ()     -- [4]
update msg = copy                       -- [5]

main : Program Flags Model Msg
main =
  Update.document                       -- [1]
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
```

1. Instead of `Browser.document`, an identically named function from `Update.Pointfree.Browser` is used. This module also exposes a version of `application`.
2. The type of `init` is now `Flags -> Init Model Msg`. The `Init` type is explained below.
3. In this example, the model is initialized without running any commands, using `initial`.
4. The type of `update` is `Msg -> Update Model Msg ()`. This return value is a type alias for `(Model -> ( Model, Cmd Msg, List () ))`.
5. Calling `copy` will just result in an `Update` that leaves the state unchanged.

### How to `Update`

TODO

### How to `Init`

TODO
