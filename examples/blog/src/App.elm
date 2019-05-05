module App exposing (..)

import App.Auth as Auth
import App.Posts as Posts
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
  let config = { flags = flags, url = url, key = key }
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

onRouteChange : Maybe Route -> State -> Update State Msg a
onRouteChange route state =
  case route of
    Just Home ->
      state
        |> update (PostsMsg (Posts.fetchCollectionMsg))
    Just (ShowPost id) ->
      state
        |> update (PostsMsg (Posts.fetchPostMsg id))
    Just (NewComment postId) ->
      state
        |> update (PostsMsg (Posts.fetchPostMsg postId))
    _ ->
      save state

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    AuthMsg authMsg ->
      state.auth
        |> Auth.update authMsg
        |> andThen (\auth -> save { state | auth = auth })
        |> mapCmd AuthMsg
    PostsMsg postsMsg ->
      state.posts
        |> Posts.update postsMsg
        |> andThen (\posts -> save { state | posts = posts })
        |> mapCmd PostsMsg
    RouterMsg routerMsg ->
      state.router
        |> Router.update { onRouteChange = onRouteChange } routerMsg
        |> andThen (\router -> save { state | router = router })
        |> mapCmd RouterMsg
        |> consumeEvents
    UiMsg uiMsg ->
      state.ui
        |> Ui.update uiMsg
        |> andThen (\ui -> save { state | ui = ui })
        |> mapCmd UiMsg

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
      Html.map AuthMsg (Auth.loginFormView auth)
    Just Register ->
      Html.map AuthMsg (Auth.registerFormView auth)
    Just NewPost ->
      Html.map PostsMsg (Posts.formView posts)
    Just Home ->
      Html.map PostsMsg (Posts.listView posts)
    Just (ShowPost id) ->
      Html.map PostsMsg (Posts.itemView posts)
    Just About ->
      div [] [ text "About" ]
    Just (NewComment postId) ->
      Html.map PostsMsg (Posts.addCommentView posts)
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
--        , text (Debug.toString state.posts)
        ]
      ]
    ]
  }
