module App.Ui exposing (..)

import App.Config exposing (..)
import Bootstrap.Navbar as Navbar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = NavbarMsg Navbar.State

type alias State =
  { navbar : Navbar.State }

init : Config -> Init State Msg
init config =
  let ( state, cmd ) = Navbar.initialState NavbarMsg
   in { navbar = state }
        |> initialWithCmd cmd

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    NavbarMsg navbarState ->
      save { state | navbar = navbarState }

subscriptions : State -> Sub Msg
subscriptions state = Navbar.subscriptions state.navbar NavbarMsg

navbarView : State -> Html Msg
navbarView state =
  Navbar.config NavbarMsg
    |> Navbar.withAnimation
    |> Navbar.brand [ href "/"] [ text "FooPress"]
    |> Navbar.items
        [ Navbar.itemLink [href "/posts/new"] [ text "Publish a post"]
        , Navbar.itemLink [href "/about"] [ text "About"] ]
    |> Navbar.view state.navbar
