module Page exposing (..)

import Data.Comment exposing (Comment)
import Data.Post exposing (Post)
import Data.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Home
import Page.Login
import Page.NewPost
import Page.Register
import Page.ShowPost
import Ui.Page
import Update.Deep exposing (..)

type Msg
  = HomePageMsg Page.Home.Msg
  | NewPostPageMsg Page.NewPost.Msg
  | ShowPostPageMsg Page.ShowPost.Msg
  | LoginPageMsg Page.Login.Msg
  | RegisterPageMsg Page.Register.Msg

type Page
  = HomePage Page.Home.State
  | NewPostPage Page.NewPost.State
  | ShowPostPage Page.ShowPost.State
  | LoginPage Page.Login.State
  | RegisterPage Page.Register.State
  | AboutPage
  | NotFoundPage

current : Page -> { isHomePage : Bool, isNewPostPage : Bool, isShowPostPage : Bool, isLoginPage : Bool, isRegisterPage : Bool, isAboutPage : Bool, isNotFoundPage : Bool }
current page =
  
  let 
      default = { isHomePage = False, isNewPostPage = False, isShowPostPage = False, isLoginPage = False, isRegisterPage = False, isAboutPage = False, isNotFoundPage = False }
   in 
      case page of
        HomePage _ ->
          { default | isHomePage = True }
        NewPostPage _ ->
          { default | isNewPostPage = True }
        ShowPostPage _ ->
          { default | isShowPostPage = True }
        LoginPage _ ->
          { default | isLoginPage = True }
        RegisterPage _ ->
          { default | isRegisterPage = True }
        AboutPage ->
          { default | isAboutPage = True }
        NotFoundPage ->
          { default | isNotFoundPage = True }

update : { onAuthResponse : Maybe Session -> a, onPostAdded : Post -> a, onCommentCreated : Comment -> a } -> Msg -> (Msg -> msg) -> Page -> Update Page msg a
update { onAuthResponse, onPostAdded, onCommentCreated } msg toMsg page = 
  case page of
    HomePage homePageState ->
      case msg of
        HomePageMsg homeMsg ->
          homePageState
            |> Page.Home.update homeMsg (toMsg << HomePageMsg)
            |> Update.Deep.map HomePage 
        _ ->
          save page
    NewPostPage newPostPageState ->
      case msg of
        NewPostPageMsg newPostMsg ->
          newPostPageState
            |> Page.NewPost.update { onPostAdded = onPostAdded } newPostMsg (toMsg << NewPostPageMsg)
            |> Update.Deep.map NewPostPage 
        _ ->
          save page
    ShowPostPage showPostPageState ->
      case msg of
        ShowPostPageMsg showPostMsg ->
          showPostPageState
            |> Page.ShowPost.update { onCommentCreated = onCommentCreated } showPostMsg (toMsg << ShowPostPageMsg)
            |> Update.Deep.map ShowPostPage 
        _ ->
          save page
    LoginPage loginPageState ->
      case msg of
        LoginPageMsg loginMsg ->
          loginPageState
            |> Page.Login.update { onAuthResponse = onAuthResponse } loginMsg (toMsg << LoginPageMsg)
            |> Update.Deep.map LoginPage 
        _ -> 
          save page
    RegisterPage registerPageState ->
      case msg of
        RegisterPageMsg registerMsg ->
          registerPageState
            |> Page.Register.update registerMsg (toMsg << RegisterPageMsg)
            |> Update.Deep.map RegisterPage 
        _ ->
          save page
    AboutPage ->
      save page
    NotFoundPage ->
      save page

subscriptions : Page -> (Msg -> msg) -> Sub msg
subscriptions page toMsg = 
  case page of
    HomePage homePageState ->
      Page.Home.subscriptions homePageState (toMsg << HomePageMsg)
    NewPostPage newPostPageState ->
      Page.NewPost.subscriptions newPostPageState (toMsg << NewPostPageMsg)
    ShowPostPage showPostPageState ->
      Page.ShowPost.subscriptions showPostPageState (toMsg << ShowPostPageMsg)
    LoginPage loginPageState ->
      Page.Login.subscriptions loginPageState (toMsg << LoginPageMsg)
    RegisterPage registerPageState ->
      Page.Register.subscriptions registerPageState (toMsg << RegisterPageMsg)
    AboutPage ->
      Sub.none
    NotFoundPage ->
      Sub.none

view : Page -> (Msg -> msg) -> Html msg
view page toMsg =
  case page of
    HomePage homePageState ->
      Page.Home.view homePageState (toMsg << HomePageMsg)
    NewPostPage newPostPageState ->
      Page.NewPost.view newPostPageState (toMsg << NewPostPageMsg)
    ShowPostPage showPostPageState ->
      Page.ShowPost.view showPostPageState (toMsg << ShowPostPageMsg)
    LoginPage loginPageState ->
      Page.Login.view loginPageState (toMsg << LoginPageMsg)
    RegisterPage registerPageState ->
      Page.Register.view registerPageState (toMsg << RegisterPageMsg)
    AboutPage ->
      Ui.Page.container "About" [ text "Welcome to Facepalm. A place to meet weird people while keeping all your personal data safe." ]
    NotFoundPage ->
      Ui.Page.container "Error 404" [ text "That means we couldn’t find that page." ]
