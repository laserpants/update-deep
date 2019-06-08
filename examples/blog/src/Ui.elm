module Ui exposing (..)

import Data.Session as Session exposing (Session)
import Page as Page exposing (Page, current)
import Bulma.Modifiers exposing (Color(..))
import Update.Deep exposing (..)
import Task
import Process
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Modifiers exposing (..)
import Maybe.Extra as Maybe

type Msg
  = ToggleBurgerMenu
  | CloseToast Int

type alias Toast =
  { message : String
  , color : Color 
  }

type alias State =
  { menuOpen : Bool 
  , toast : Maybe ( Int, Toast )
  , toastCounter : Int
  }

incrementToastCounter : State -> Update State msg a
incrementToastCounter state = save { state | toastCounter = 1 + state.toastCounter }

toggleMenuOpen : State -> Update State msg a
toggleMenuOpen state = save { state | menuOpen = not state.menuOpen }

closeBurgerMenu : State -> Update State msg a
closeBurgerMenu state = save { state | menuOpen = False }

setToast : Toast -> State -> Update State msg a
setToast toast state = save { state | toast = Just ( state.toastCounter, toast ) }

showToast : Toast -> (Msg -> msg) -> State -> Update State msg a
showToast toast toMsg state = 
  state 
    |> setToast toast
    |> andAddCmd (Task.perform (CloseToast state.toastCounter |> always) (Process.sleep 4000))
    |> andThen incrementToastCounter
    |> mapCmd toMsg 

showInfoToast : String -> (Msg -> msg) -> State -> Update State msg a
showInfoToast message = showToast { message = message, color = Info } 

closeToast : State -> Update State msg a
closeToast state = save { state | toast = Nothing }

init : Update State msg a
init = 
  save State
    |> andMap (save False)
    |> andMap (save Nothing)
    |> andMap (save 1)

update : Msg -> (Msg -> msg) -> State -> Update State msg a
update msg toMsg =
  case msg of
    ToggleBurgerMenu ->
      toggleMenuOpen
    CloseToast id ->
      pluck .toast (\toast -> 
        case toast of
          Nothing ->
            save 
          Just ( toastId, _ ) ->
            if id == toastId then closeToast else save) 

toastContainer : Html msg -> Html msg
toastContainer html = div [] [ html ]

--toastContainer : Html msg -> Html msg
--toastContainer html =
--  Html.Styled.div 
--    [ Html.Styled.Attributes.css
--      [ Css.width (Css.pct 100)
--      , Css.position (Css.fixed)
--      , Css.bottom (Css.px 0)
--      , Css.pointerEvents (Css.none)
--      , Css.displayFlex
--      , Css.flexDirection (Css.column)
--      , Css.padding (Css.px 15)
--      , Css.Media.withMedia 
--        [ Css.Media.only Css.Media.screen 
--          [ Css.Media.minWidth (Css.px 768) ] 
--        ] [ Css.alignItems (Css.start) ]
--      , Css.zIndex (Css.int 9000)
--      ]
--    ] [ Html.Styled.fromUnstyled html ]
--  |> Html.Styled.toUnstyled

toastMessage : State -> (Msg -> msg) -> Html msg
toastMessage { toast } toMsg =
  case toast of
    Nothing ->
      text ""
    Just ( id, { message, color } ) ->
      notificationWithDelete color [] (CloseToast id) [ text message ]
        |> toastContainer 
        |> Html.map toMsg

navbar : Maybe Session -> Page -> State -> (Msg -> msg) -> Html msg
navbar session page { menuOpen } toMsg = 

  let 
      burger = 
        navbarBurger menuOpen [ class "has-text-white", onClick ToggleBurgerMenu ] 
          [ span [] [], span [] [], span [] [] ]

      buttons =
        if Maybe.isNothing session
            then
              [ p [ class "control" ] 
                [ a [ class "button is-primary", href "/register" ] [ text "Register" ] ] 
              , p [ class "control" ] 
                [ a [ class "button is-light", href "/login" ] [ text "Log in" ] ] 
              ] 
            else
              [ p [ class "control" ] 
                [ a [ class "button is-primary", href "/logout" ] [ text "Log out" ] ] 
              ] 

      currentPage = current page
   in
        fixedNavbar Top { navbarModifiers | color = Info } []
          [ navbarBrand [] burger
            [ navbarItem False [] [ h5 [ class "title is-5" ] [ a [ class "has-text-white", href "/" ] [ text "Facepalm" ] ] ] ]
          , navbarMenu menuOpen []
            [ navbarStart [ class "is-unselectable" ] 
              [ navbarItemLink currentPage.isHomePage [ href "/" ] [ text "Home" ]
              , navbarItemLink currentPage.isAboutPage [ href "/about" ] [ text "About" ]
              , navbarItemLink currentPage.isNewPostPage [ href "/posts/new" ] [ text "New post" ]
              ]
              , navbarEnd [] 
                [ navbarItem False [] [ div [ class "field is-grouped" ] buttons ] ]
            ]
          ]

        |> Html.map toMsg
