module App exposing (..)

import Maybe.Extra as Maybe
import Browser.Navigation as Navigation
import Data.Post as Post exposing (Post)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.Comment as Comment exposing (Comment)
import Ui exposing (showToast, showInfoToast)
import Page exposing (Page)
import Page.Home
import Page.ShowPost
import Page.NewPost
import Page.Register
import Page.Login
import Url exposing (Url)
import Update.Deep exposing (..)
import Json.Decode as Json
import Bulma.Modifiers exposing (..)
import Ports
import Browser exposing (Document, UrlRequest)
import Bulma.Layout exposing (SectionSpacing(..), hero,heroBody,container, fluidContainer)
import Route exposing (Route(..), fromUrl)
import Update.Deep.Router as Router

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
                |> andThen (inUi (showToast { message = "You must be logged in to access that page.", color = Warning } UiMsg))
            else 
              state
                |> gotoPage
                |> mapCmd PageMsg

      unlessAuthenticated gotoPage =
        unwrap .session (\session -> if Nothing /= session then redirect "/" else gotoPage >> mapCmd PageMsg)

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
            >> andThen (inUi (showInfoToast "You have been logged out" UiMsg))

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
returnToRestrictedUrl = unwrap .restrictedUrl (redirect << Maybe.withDefault "/") 

handleAuthResponse : Maybe Session -> State -> Update State Msg a
handleAuthResponse maybeSession = 

  let 
      authenticated = always (Maybe.isJust maybeSession)
   in 
      setSession maybeSession
        >> andThen (updateSessionStorage maybeSession)
        >> andThenIf authenticated returnToRestrictedUrl

handlePostAdded : Post -> State -> Update State Msg a
handlePostAdded post =
  redirect "/" >> andThen (inUi (showInfoToast "Your post was published" UiMsg))

handleCommentCreated : Comment -> State -> Update State Msg a
handleCommentCreated comment = 
  inUi (showInfoToast "Your comment was successfully received" UiMsg) 

update : Msg -> State -> Update State Msg a
update msg =
  case msg of
    RouterMsg routerMsg ->
      inRouter (Router.update { onRouteChange = handleRouteChange } routerMsg)
    PageMsg pageMsg ->
      inPage (Page.update 
        { onAuthResponse = handleAuthResponse
        , onPostAdded = handlePostAdded
        , onCommentCreated = handleCommentCreated
        } pageMsg PageMsg)
    UiMsg uiMsg ->
      inUi (Ui.update uiMsg UiMsg)

subscriptions : State -> Sub Msg
subscriptions { page } = Page.subscriptions page PageMsg

view : State -> Document Msg
view { page, session, ui } = 
  { title = "Welcome to Facepalm"
  , body =
    [ Ui.navbar session page ui UiMsg
    , Ui.toastMessage ui UiMsg
    , Bulma.Layout.section NotSpaced [] [ Page.view page PageMsg ] 
    ] 
  }
