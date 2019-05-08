module Main exposing (..)

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url exposing (Url)

type Page
  = HomePage
  | AboutPage
  | PostCreatePage

--

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest

type alias Flags = ()

type alias Model =
  { page : Page }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key = ( { page = HomePage }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

view : Model -> Document Msg
view model =
  { title = ""
  , body = 
    [ div [] 
      [ text "Hello" ]
    ]
  }

main : Program Flags Model Msg
main =
  Browser.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = UrlChange
    , onUrlRequest  = UrlRequest }
