module App.Config exposing (..)

import Browser.Navigation as Navigation
import Flags exposing (..)

type alias Config =
  { flags : Flags
  , key   : Navigation.Key }
