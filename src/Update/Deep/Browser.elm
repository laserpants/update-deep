module Update.Deep.Browser exposing (application, document)

{-|

@docs application, document

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Update.Deep exposing (Update, applicationInit, documentInit, runUpdate)
import Url exposing (Url)


{-| Used in the same way as `Browser.application`, but instead creates a `Program`
where `init` and `update` are based on the `Update` type of this library.
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


{-| Used in the same way as `Browser.document`, but instead creates a `Program`
where `init` and `update` are based on the `Update` type of this library.
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
