# Update Eventful

TODO

## How to use this library

```elm
module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Update.Eventful exposing (Update, copy)
import Update.Eventful.Browser as Update

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

-- The above code is just business as usual. The only real difference is in the init, update, and
-- main calls below.

init : Flags -> Update Model Msg ()              -- [2]
init _ = copy {}                                 -- [3]

update : Msg -> Model -> Update Model Msg ()     -- [4]
update msg = copy                                -- [5]

main : Program Flags Model Msg
main =
  Update.document                                -- [1]
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
```

1. Instead of `Browser.document`, a function of the same name from `Update.Eventful.Browser` is used. This module also exposes its own version of `application`.
2. The type of `init` is now `Flags -> Update Model Msg ()`. The `Update` type is explained below.
3. In this example, the model is initialized without running any commands.
4. The type of `update` has become `Msg -> Update Model Msg e`. This return value is a type alias for `( Model, Cmd Msg, List e )`. The `e` parameter represents an *event* type. Events are not used in this example; hence the `()` unit type.
5. Calling `copy` will result in an `Update` that leaves the state unchanged.



### How to `Update`

TODO

### How to use events

TODO
