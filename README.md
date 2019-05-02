# Update Deep

TODO

```
                                               ||             ┌────────────┐
                                               ||             │    Main    │
                ┌────────────┐                 ||             └── ▲ ─ ▲ ───┘
           ┌────│    Main    │────┐            ||                 │   │
           │    └────────────┘    │            ||                 │   │--- onItemAdded
           │                      │            ||   onTaskDone ---│   │
  ┌─────── ▼ ───────┐       ┌──── ▼ ────┐      ||                 │   │   ┌───────────┐
  │  Notifications  │       │   Todos   │      ||                 └───┴───│   Todos   │
  └─────────────────┘       └─────┬─────┘      ||                         └──── ▲ ────┘
                                  │            ||                               │
                                  │            ||                               │--- onSubmit
                          ┌────── ▼ ──────┐    ||                               │
                          │   TodosForm   │    ||                       ┌───────┴───────┐
                          └───────────────┘    ||                       │   TodosForm   │
                                               ||                       └───────────────┘
```

<!--
## How to use this library

```elm
module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Update.Deep exposing (Update, save)
import Update.Deep.Browser as Update

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

init : Flags -> Init Model Msg                   -- [2]
init _ = initial {}                              -- [3]

update : Msg -> Model -> Update Model Msg a      -- [4]
update msg model = save model                    -- [5]

main : Program Flags Model Msg
main =
  Deep.document                                  -- [1]
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
```

1. Instead of `Browser.document`, a function of the same name from `Update.Deep.Browser` is used. This module also exposes its own version of `application`.
2. The type of `init` is now `Flags -> Init Model Msg`. The `Init` type is explained below.
3. In this example, the model is initialized without running any commands. Normally, you'd have returned `( {}, Cmd.none )` to achieve the same thing.
4. The type of `update` has become `Msg -> Model -> Update Model Msg a`. This return value is a type alias for `( Model, Cmd Msg, List a )`. The purpose of the last element of this tuple, and the `a` parameter, is to make it possible for callbacks to be passed down through the update hierarchy.
5. Calling `save` here takes the model and wraps it in an `Update`.
-->
