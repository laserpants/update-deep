module Update.Deep.Browser exposing (document, application)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Update.Deep exposing (Init, Update, documentInit, applicationInit, runUpdate)
import Url exposing (Url)

document : { a | init : flags -> Init m c
               , subscriptions : m -> Sub c
               , update : c -> m -> Update m c e
               , view : m -> Document c } -> Program flags m c
document { init, update, subscriptions, view } =
  Browser.document
    { init          = documentInit init
    , update        = runUpdate update
    , subscriptions = subscriptions
    , view          = view }

application : { a | init : flags -> Url -> Navigation.Key -> Init m c
                  , onUrlChange : Url -> c
                  , onUrlRequest : UrlRequest -> c
                  , subscriptions : m -> Sub c
                  , update : c -> m -> Update m c e
                  , view : m -> Document c } -> Program flags m c
application { init, update, subscriptions, view, onUrlChange, onUrlRequest } =
  Browser.application
    { init          = applicationInit init
    , update        = runUpdate update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
