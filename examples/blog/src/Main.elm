module Main exposing (..)

import Maybe.Extra as Maybe
import Browser.Navigation as Navigation
import Data.Post as Post exposing (Post)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.Comment as Comment exposing (Comment)
import Ui 
import Page exposing (Page)
import Page.Home
import Page.ShowPost
import Page.NewPost
import Page.Register
import Page.Login
import Router
import Url.Parser as Parser exposing (Parser, parse, oneOf, (</>))
import Url exposing (Url)
import Update.Deep exposing (..)
import Update.Deep.Browser as Deep
import Json.Decode as Json
import Bulma.Modifiers exposing (..)
import Ports
import Browser exposing (Document, UrlRequest)
import Bulma.Layout exposing (SectionSpacing(..), hero,heroBody,container, fluidContainer)

--import Dict exposing (Dict)
--import Form exposing (Form)
--import Form.Error exposing (ErrorValue(..), Error)
--import Form.Field as Field exposing (Field, FieldValue(..))
--import Form.Input as Input
--import Form.Validate as Validate exposing (Validation, succeed, field)
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)
--import Http exposing (emptyBody)
--import Json.Encode exposing (object)
--import Ports
--import Bulma.Elements as Bulma exposing (TitleSize(..), title)
--import Bulma.Components exposing (..)
--import Bulma.Form exposing (controlInputModifiers, controlTextAreaModifiers)
--import Css 
--import Css.Media
--import Html
--import Html.Styled 
--import Html.Styled.Attributes 
--import Html.Styled.Events 
--import Task
--import Process

--

--

--

--

--

type Route
  = Home
  | Login
  | Logout
  | Register
  | About
  | NewPost
  | ShowPost Int

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home (Parser.top)
    , Parser.map Login (Parser.s "login")
    , Parser.map Logout (Parser.s "logout")
    , Parser.map Register (Parser.s "register")
    , Parser.map About (Parser.s "about")
    , Parser.map NewPost (Parser.s "posts" </> Parser.s "new")
    , Parser.map ShowPost (Parser.s "posts" </> Parser.int)
    ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser

--

type alias Flags =
  { api     : String
  , session : String }

type Msg
  = RouterMsg Router.Msg
  | PageMsg Page.Msg
  | UiMsg Ui.Msg

type alias State =
  { session : Maybe Session 
  , router : Router.State Route
  , ui : Ui.State
  , restrictedUrl : Maybe String
  , page : Page
  }

setRestrictedUrl : String -> State -> Update State msg a
setRestrictedUrl url state = save { state | restrictedUrl = Just url }

resetRestrictedUrl : State -> Update State msg a
resetRestrictedUrl state = save { state | restrictedUrl = Nothing }

setSession : Maybe Session -> State -> Update State msg a
setSession session state = save { state | session = session }

inRouter : In State (Router.State Route) msg a
inRouter =
    inState { get = .router, set = \state router -> { state | router = router } }

inUi : In State Ui.State msg a
inUi =
    inState { get = .ui, set = \state ui -> { state | ui = ui } }

inPage : In State Page msg a
inPage =
    inState { get = .page, set = \state page -> { state | page = page } }

initSession : Flags -> Maybe Session
initSession { session } =
  case Json.decodeString Session.decoder session of
    Ok result ->
      Just result
    _ ->
      Nothing

init : Flags -> Url -> Navigation.Key -> Update State Msg a
init flags url key = 
  save State
    |> andMap (initSession flags |> save)
    |> andMap (Router.init fromUrl key RouterMsg)
    |> andMap Ui.init
    |> andMap (save Nothing)
    |> andMap (save Page.NotFoundPage)
    |> andThen (update (RouterMsg (Router.UrlChange url)))

redirect : String -> State -> Update State msg a
redirect = inRouter << Router.redirect

loadPage : Update Page msg (State -> Update State msg a) -> State -> Update State msg a
loadPage setPage state = 

  let 
      isLoginRoute = always (Just Login == state.router.route)
   in 
      state
        |> inPage (always setPage)
        |> andThenIf (not << isLoginRoute) resetRestrictedUrl
        |> andThen (inUi Ui.closeBurgerMenu)

handleRouteChange : Url -> Maybe Route -> State -> Update State Msg a
handleRouteChange url maybeRoute =

  let 
      ifAuthenticated gotoPage state =
        if Nothing == state.session 
            then 
              state
                |> setRestrictedUrl url.path  -- Redirect back here after successful login
                |> andThen (redirect "/login")
                |> andThen (inUi (Ui.showToast { message = "You must be logged in to access that page.", color = Warning } UiMsg))
            else 
              state
                |> gotoPage
                |> mapCmd PageMsg

      unlessAuthenticated gotoPage =
        pluck .session (\session -> if Nothing /= session then redirect "/" else gotoPage >> mapCmd PageMsg)

   in 
      case maybeRoute of

        -- No route
        Nothing ->
          loadPage (save Page.NotFoundPage) >> mapCmd PageMsg

        -- Authenticated only
        Just NewPost ->
          ifAuthenticated (Page.NewPost.init Page.NewPostPageMsg |> Update.Deep.map Page.NewPostPage |> loadPage) 

        -- Redirect if already authenticated
        Just Login ->
          unlessAuthenticated (Page.Login.init Page.LoginPageMsg |> Update.Deep.map Page.LoginPage |> loadPage) 

        -- Redirect if already authenticated
        Just Register ->
          unlessAuthenticated (Page.Register.init Page.RegisterPageMsg |> Update.Deep.map Page.RegisterPage |> loadPage) 

        -- Other
        Just (ShowPost id) ->
          mapCmd PageMsg << (
            Page.ShowPost.init id Page.ShowPostPageMsg 
              |> andThen (Page.ShowPost.update { onCommentCreated = always save } Page.ShowPost.FetchPost Page.ShowPostPageMsg) 
              |> Update.Deep.map Page.ShowPostPage
              |> loadPage)  

        Just Home ->
          mapCmd PageMsg << (
            Page.Home.init Page.HomePageMsg 
              |> andThen (Page.Home.update Page.Home.FetchPosts Page.HomePageMsg) 
              |> Update.Deep.map Page.HomePage
              |> loadPage) 

        Just Logout ->
          setSession Nothing
            >> andThen (updateSessionStorage Nothing)
            >> andThen (redirect "/")
            >> andThen (inUi (Ui.showInfoToast "You have been logged out" UiMsg))

        Just About ->
          loadPage (save Page.AboutPage) >> mapCmd PageMsg

updateSessionStorage : Maybe Session -> State -> Update State msg a
updateSessionStorage maybeSession =
  case maybeSession of
    Nothing ->
      addCmd (Ports.clearSession ())
    Just session ->
      addCmd (Ports.setSession session)

returnToRestrictedUrl : State -> Update State Msg a
returnToRestrictedUrl = pluck .restrictedUrl (redirect << Maybe.withDefault "/") 

handleAuthResponse : Maybe Session -> State -> Update State Msg a
handleAuthResponse maybeSession = 

  let 
      authenticated = always (Maybe.isJust maybeSession)
   in 
      setSession maybeSession
        >> andThen (updateSessionStorage maybeSession)
        >> andThenIf authenticated returnToRestrictedUrl

update : Msg -> State -> Update State Msg a
update msg =
  case msg of
    RouterMsg routerMsg ->
      inRouter (Router.update { onRouteChange = handleRouteChange } routerMsg)
    PageMsg pageMsg ->
      inPage (Page.update 
        { onAuthResponse = handleAuthResponse
        , onPostAdded = always (redirect "/" >> andThen (inUi (Ui.showInfoToast "Your post was published" UiMsg)))
        , onCommentCreated = inUi (Ui.showInfoToast "Your comment was received" UiMsg) |> always 
        } pageMsg PageMsg)
    UiMsg uiMsg ->
      inUi (Ui.update uiMsg UiMsg)

subscriptions : State -> Sub Msg
subscriptions { page } = Page.subscriptions page PageMsg

view : State -> Document Msg
view ({ page, session, ui } as state) = 
  { title = "Welcome to Facepalm"
  , body =
    [ Ui.navbar session page ui UiMsg
    , Ui.toastMessage ui UiMsg
    , Bulma.Layout.section NotSpaced [] 
      [ Page.view page PageMsg ] 
    ] 
  }

main : Program Flags State Msg
main =
  Deep.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = RouterMsg << Router.UrlChange
    , onUrlRequest  = RouterMsg << Router.UrlRequest
    }
