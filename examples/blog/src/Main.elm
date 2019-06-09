module Main exposing (main)

import App exposing (..)
import Update.Deep.Router as Router
import Update.Deep.Browser exposing (application)


main : Program Flags State Msg
main =
    application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = RouterMsg << Router.UrlChange
        , onUrlRequest = RouterMsg << Router.UrlRequest
        }
