module Update.Deep.Browser exposing (application, document)

{-|

@docs application, document

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Update.Deep exposing (Update, applicationInit, documentInit, runUpdate)
import Url exposing (Url)


{-| Used as a replacement for `Browser.application`, but instead creates a `Program`
where `init` and `update` are based on the `Update` type of this library.

```
init : flags -> Url -> Navigation.Key -> Update model msg a
update : msg -> model -> Update model msg a
```

See the [blog application](https://github.com/laserpants/elm-update-deep/tree/master/examples/blog) for an example of how to use this in a program.
-}
application :
    { init : flags -> Url -> Navigation.Key -> Update model msg a
    , onUrlChange : Url -> msg
    , onUrlRequest : UrlRequest -> msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> Update model msg a
    , view : model -> Document msg
    }
    -> Program flags model msg
application config =
    Browser.application
        { init = applicationInit config.init
        , update = runUpdate config.update
        , subscriptions = config.subscriptions
        , view = config.view
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        }


{-| Used as a replacement for `Browser.document`, but instead creates a `Program`
where `init` and `update` are based on the `Update` type of this library.

```
init : flags -> Update model msg a
update : msg -> model -> Update model msg a
```

See the [todo-list application](https://github.com/laserpants/elm-update-deep/tree/master/examples/todo-list) for an example of how to use this in a program.
-}
document :
    { init : flags -> Update model msg a
    , subscriptions : model -> Sub msg
    , update : msg -> model -> Update model msg a
    , view : model -> Document msg
    }
    -> Program flags model msg
document config =
    Browser.document
        { init = documentInit config.init
        , update = runUpdate config.update
        , subscriptions = config.subscriptions
        , view = config.view
        }
