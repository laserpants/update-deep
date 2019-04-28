module Update.Pointfree.Browser exposing (document, application)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Update.Pointfree exposing (Init, Update, documentInit, applicationInit, runUpdate)
import Url exposing (Url)

document : { a | init : flags -> Init model msg
               , subscriptions : model -> Sub msg
               , update : msg -> Update model msg e
               , view : model -> Document msg }
        -> Program flags model msg
document { init, update, subscriptions, view } =
  Browser.document
    { init          = documentInit init
    , update        = runUpdate << update
    , subscriptions = subscriptions
    , view          = view }

type alias Config flags = { flags : flags, key : Navigation.Key, url : Url }

application : { a | init : Config flags -> Init model msg
                  , onUrlChange : Url -> msg
                  , onUrlRequest : UrlRequest -> msg
                  , subscriptions : model -> Sub msg
                  , update : msg -> Update model msg e
                  , view : model -> Document msg }
           -> Program flags model msg
application { init, update, subscriptions, view, onUrlChange, onUrlRequest } =
  Browser.application
    { init          = applicationInit init
    , update        = runUpdate << update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
