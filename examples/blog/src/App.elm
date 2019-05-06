module App exposing (..)

import App.Auth as Auth
import App.Auth.Login.Page
import App.Auth.Register.Page
import App.Comments.Page
import App.Posts as Posts
import App.Posts.Create.Page
import App.Posts.Item.Page
import App.Posts.Item.Page
import App.Posts.List.Page
import App.Route exposing (..)
import App.Router as Router
import App.Ui as Ui
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Flags exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)
import Url exposing (Url)

type Msg
  = AuthMsg Auth.Msg
  | PostsMsg Posts.Msg
  | RouterMsg Router.Msg
  | UiMsg Ui.Msg

type alias State =
  { auth   : Auth.State
  , posts  : Posts.State
  , router : Router.State
  , ui     : Ui.State }

initUrl : Url -> Init State Msg -> Init State Msg
initUrl url initState =
  let msg = RouterMsg (Router.UrlChange url)
   in initToUpdate initState
    |> andThen (update msg)
    |> updateToInit

init : Flags -> Url -> Navigation.Key -> Init State Msg
init flags url key =
  let config = { flags = flags, key = key }
      auth   = Auth.init config
      posts  = Posts.init config
      router = Router.init config
      ui     = Ui.init config
   in { auth   = auth.state
      , posts  = posts.state
      , router = router.state
      , ui     = ui.state }
        |> initial
        |> initCmd AuthMsg auth
        |> initCmd PostsMsg posts
        |> initCmd RouterMsg router
        |> initCmd UiMsg ui
        |> initUrl url

insertAsAuthIn : State -> Auth.State -> Update State Msg a
insertAsAuthIn state auth = save { state | auth = auth }

insertAsPostsIn : State -> Posts.State -> Update State Msg a
insertAsPostsIn state posts = save { state | posts = posts }

insertAsRouterIn : State -> Router.State -> Update State Msg a
insertAsRouterIn state router = save { state | router = router }

insertAsUiIn : State -> Ui.State -> Update State Msg a
insertAsUiIn state ui = save { state | ui = ui }

onRouteChange : Maybe Route -> State -> Update State Msg a
onRouteChange route state =
  case route of
    Just Home ->
      state.posts
        |> updateSubstate postsState (Posts.FetchAll)
        |> andThen (insertAsPostsIn state)
    Just (ShowPost id) ->
      state.posts
        |> updateSubstate postsState (Posts.SetPage id)
        |> andThen (insertAsPostsIn state)
    Just (NewComment postId) ->
      state.posts
        |> updateSubstate postsState (Posts.SetPage postId)
        |> andThen (insertAsPostsIn state)
    _ ->
      save state

type alias AppSubstate c a e = Substate Msg c a e

authState : AppSubstate Auth.Msg Auth.State a
authState = { update = Auth.update, msgCons = AuthMsg }

postsState : AppSubstate Posts.Msg Posts.State a
postsState = { update = Posts.update, msgCons = PostsMsg }

routerState : AppSubstate Router.Msg Router.State (State -> Update State Msg e)
routerState =
  { update  = Router.update { onRouteChange = onRouteChange }
  , msgCons = RouterMsg }

uiState : AppSubstate Ui.Msg Ui.State a
uiState = { update = Ui.update, msgCons = UiMsg }

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    AuthMsg authMsg ->
      state.auth
        |> updateSubstate authState authMsg
        |> andThen (insertAsAuthIn state)
    PostsMsg postsMsg ->
      state.posts
        |> updateSubstate postsState postsMsg
        |> andThen (insertAsPostsIn state)
    RouterMsg routerMsg ->
      state.router
        |> updateSubstate routerState routerMsg
        |> andThen (insertAsRouterIn state)
        |> consumeEvents
    UiMsg uiMsg ->
      state.ui
        |> updateSubstate uiState uiMsg
        |> andThen (insertAsUiIn state)

subscriptions : State -> Sub Msg
subscriptions state =
  Sub.batch
    [ Sub.map AuthMsg (Auth.subscriptions state.auth)
    , Sub.map PostsMsg (Posts.subscriptions state.posts)
    , Sub.map RouterMsg (Router.subscriptions state.router)
    , Sub.map UiMsg (Ui.subscriptions state.ui) ]

pageOutlet : State -> Html Msg
pageOutlet { auth, posts, router } =
  case router.route of
    Just Login ->
      Html.map (AuthMsg << Auth.LoginMsg) (App.Auth.Login.Page.view auth.loginPage)
    Just Register ->
      Html.map (AuthMsg << Auth.RegisterMsg) (App.Auth.Register.Page.view auth.registerPage)
    Just NewPost ->
      Html.map (PostsMsg << Posts.CreateMsg) (App.Posts.Create.Page.view posts.createPage)
    Just Home ->
      Html.map (PostsMsg << Posts.ListMsg) (App.Posts.List.Page.view posts.listPage)
    Just (ShowPost id) ->
      Html.map (PostsMsg << Posts.ItemMsg) (App.Posts.Item.Page.view posts.itemPage)
    Just About ->
      div [] [ text "About" ]
    Just (NewComment postId) ->
      Html.map (PostsMsg << Posts.ItemMsg << App.Posts.Item.Page.CommentsMsg) (App.Comments.Page.view posts.itemPage.commentsPage)
    Nothing ->
      div [] [ text "Don't know that page" ]

view : State -> Document Msg
view state =
  { title = ""
  , body  =
    [ div []
      [ ul []
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/about" ] [ text "About" ] ]
        , li [] [ a [ href "/login" ] [ text "Login" ] ]
        , li [] [ a [ href "/register" ] [ text "Register" ] ]
        , li [] [ a [ href "/posts/1" ] [ text "Show post" ] ]
        , li [] [ a [ href "/posts/new" ] [ text "New post" ] ]
        , li [] [ a [ href "/posts/1/comments/new" ] [ text "New comment" ] ]
        , li [] [ a [ href "#" ] [ text "#" ] ]
        , pageOutlet state
        , text (Debug.toString state)
        ]
      ]
    ]
  }
