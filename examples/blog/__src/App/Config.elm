module App.Config exposing (..)

import Browser.Navigation as Navigation
import Flags exposing (..)
import Url exposing (Url)

type alias Config =
  { flags : Flags
  , key   : Navigation.Key }
