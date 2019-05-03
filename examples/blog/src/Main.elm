module Main exposing (..)

import App exposing (Msg(..), State)
import App.Router
import Flags exposing (..)
import Update.Deep.Browser as Deep

main : Program Flags State Msg
main =
  Deep.application
    { init          = App.init
    , update        = App.update
    , subscriptions = App.subscriptions
    , view          = App.view
    , onUrlChange   = RouterMsg << App.Router.UrlChange
    , onUrlRequest  = RouterMsg << App.Router.UrlRequest }
