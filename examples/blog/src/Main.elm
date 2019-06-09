module Main exposing (main)

import App exposing (..)
import Update.Deep.Browser exposing (application)
import Update.Deep.Router as Router


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
