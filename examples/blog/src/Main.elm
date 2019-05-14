module Main exposing (..)

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Form exposing (Form)
import Form.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (emptyBody)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode exposing (object)
import UiFormView
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, parse, oneOf, (</>))

--
--
--

type alias Update m a c = ( m, Cmd c, List a )

save : m -> Update m a c
save model = ( model, Cmd.none, [] )

runCmd : Cmd c -> m -> Update m a c
runCmd cmd model = ( model, cmd, [] )

mapCmd : (c -> d) -> Update m a c -> Update m a d
mapCmd f ( model, cmd, events ) = ( model, Cmd.map f cmd, events )

invokeHandler : a -> m -> Update m a c
invokeHandler handler model = ( model, Cmd.none, [ handler ] )

ap : Update (m -> n) a c -> Update m a c -> Update n a c
ap ( f, cmda, e ) ( model, cmdb, e2 ) = ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

map : (m -> n) -> Update m a c -> Update n a c
map f ( model, cmd, events ) = ( f model, cmd, events )

map2 : (m -> n -> o) -> Update m a c -> Update n a c -> Update o a c
map2 f = ap << map f

map3 : (m -> n -> o -> p) -> Update m a c -> Update n a c -> Update o a c -> Update p a c
map3 f x = ap << map2 f x

map4 : (m -> n -> o -> p -> q) -> Update m a c -> Update n a c -> Update o a c -> Update p a c -> Update q a c
map4 f x y = ap << map3 f x y

join : Update (Update m a c) a c -> Update m a c
join ( ( model, cmda, e ), cmdb, e2 ) = ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

andThen : (m -> Update n a c) -> Update m a c -> Update n a c
andThen f = join << map f

kleisli : (n -> Update o a c) -> (m -> Update n a c) -> m -> Update o a c
kleisli f g = andThen f << g

andRunCmd : Cmd c -> Update m a c -> Update m a c
andRunCmd = andThen << runCmd

andInvokeHandler : a -> Update m a c -> Update m a c
andInvokeHandler = andThen << invokeHandler

foldEvents : Update m (m -> Update m a c) c -> Update m a c
foldEvents ( model, cmd, events ) = List.foldr andThen ( model, cmd, [] ) events

message_ : ((n -> Update n b d) -> d) -> { get : n -> m , set : n -> m -> n , update : c -> m -> Update m (n -> Update n b d) c } -> c -> n -> Update n b d
message_ cons { update, get, set } msg model =
  model
    |> get
    |> update msg
    |> mapCmd (message cons { update = update, get = get, set = set })
    |> map (set model)
    |> foldEvents

message : ((n -> Update n b c) -> c) -> { update : a -> m -> Update m (n -> Update n b c) a, get : n -> m, set : n -> m -> n } -> a -> c
message cons access = cons << message_ cons access

applicationInit : (d -> e -> f -> Update m a c) -> d -> e -> f -> ( m, Cmd c )
applicationInit f a b c = let ( model, cmd, _ ) = f a b c in ( model, cmd )

documentInit : (f -> Update m a c) -> f -> ( m, Cmd c )
documentInit f a = let ( model, cmd, _ ) = f a in ( model, cmd )

runUpdate : (d -> e -> Update m a c) -> d -> e -> ( m, Cmd c )
runUpdate f a b = let ( model, cmd, _ ) = f a b in ( model, cmd )

--

application config = 
  Browser.application
    { init          = applicationInit config.init
    , update        = runUpdate config.update
    , subscriptions = config.subscriptions
    , view          = config.view
    , onUrlChange   = config.onUrlChange
    , onUrlRequest  = config.onUrlRequest }

document config = 
  Browser.document
    { init          = documentInit config.init
    , update        = runUpdate config.update
    , subscriptions = config.subscriptions
    , view          = config.view }

--
--
--

type Route
  = Home
  | About
  | PostCreate
  | Post Int
  | CommentPost Int
  | Login
  | Register

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home        (Parser.top)
    , Parser.map About       (Parser.s "about")
    , Parser.map PostCreate  (Parser.s "posts" </> Parser.s "new")
    , Parser.map Post        (Parser.s "posts" </> Parser.int)
    , Parser.map CommentPost (Parser.s "posts" </> Parser.int </> Parser.s "comment")
    , Parser.map Login       (Parser.s "login")
    , Parser.map Register    (Parser.s "register") ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser

--

type alias RouterUpdate a = RouterModel -> Update RouterModel a (RouterMsg a)

type RouterMsg a
  = UrlChange Url
  | UrlRequest UrlRequest
  | Redirect String

type alias RouterModel =
  { route : Maybe Route
  , key   : Navigation.Key }

setRoute : Maybe Route -> RouterModel -> Update RouterModel a (RouterMsg a)
setRoute route model = save { model | route = route }

routerInit : Navigation.Key -> Update RouterModel (RouterUpdate a) (RouterMsg a)
routerInit key =
  save
    { route = Nothing
    , key   = key }

routerUpdate : { t | onRouteChange : Maybe Route -> a } -> RouterMsg a -> RouterUpdate a
routerUpdate { onRouteChange } msg model =
  case msg of
    UrlChange url ->
      let route = fromUrl url
       in model
        |> setRoute route
        |> andInvokeHandler (onRouteChange route)
    UrlRequest (Browser.Internal url) ->
      model
        |> runCmd (Navigation.pushUrl model.key (Url.toString url))
    UrlRequest (Browser.External href) ->
      model
        |> runCmd (Navigation.load href)
    Redirect href ->
      model
        |> runCmd (Navigation.replaceUrl model.key href)

routerSubscriptions : RouterModel -> Sub (RouterMsg a)
routerSubscriptions model = Sub.none

--

type alias UiUpdate a = UiModel -> Update UiModel a (UiMsg a)

type UiMsg a
  = NoUiMsg

type alias UiModel =
  {}

uiInit : Update UiModel (UiUpdate a) (UiMsg a)
uiInit = save {}

uiUpdate : UiMsg a -> UiModel -> Update UiModel a (UiMsg a)
uiUpdate msg model = save model

uiSubscriptions : UiModel -> Sub (UiMsg a)
uiSubscriptions model = Sub.none

--

type alias AuthLoginUpdate a = AuthLoginModel -> Update AuthLoginModel a (AuthLoginMsg a)

type AuthLoginMsg a
  = NoAuthLoginMsg

type alias AuthLoginModel =
  {}

authLoginUpdate : AuthLoginMsg a -> AuthLoginModel -> Update AuthLoginModel a (AuthLoginMsg a)
authLoginUpdate msg model = save model

authLoginSubscriptions : AuthLoginModel -> Sub (AuthLoginMsg a)
authLoginSubscriptions model = Sub.none

--

type alias AuthRegisterUpdate a = AuthRegisterModel -> Update AuthRegisterModel a (AuthRegisterMsg a)

type AuthRegisterMsg a
  = NoAuthRegisterMsg

type alias AuthRegisterModel =
  {}

authRegisterUpdate : AuthRegisterMsg a -> AuthRegisterModel -> Update AuthRegisterModel a (AuthRegisterMsg a)
authRegisterUpdate msg model = save model

authRegisterSubscriptions : AuthRegisterModel -> Sub (AuthRegisterMsg a)
authRegisterSubscriptions model = Sub.none

--

type alias PostShowUpdate a = PostShowModel -> Update PostShowModel a (PostShowMsg a)

type PostShowMsg a
  = NoPostShowMsg

type alias PostShowModel =
  {}

postShowInit : Update PostShowModel (PostShowUpdate a) (PostShowMsg a)
postShowInit = save {}

postShowUpdate : PostShowMsg a -> PostShowModel -> Update PostShowModel a (PostShowMsg a)
postShowUpdate msg model =
  case msg of
    _ -> save model

postShowSubscriptions : PostShowModel -> Sub (PostShowMsg a)
postShowSubscriptions model = Sub.none

--

type alias PostCommentUpdate a = PostCommentModel -> Update PostCommentModel a (PostCommentMsg a)

type PostCommentMsg a
  = NoPostCommentMsg

type alias PostCommentModel =
  {}

postCommentInit : Update PostCommentModel (PostCommentUpdate a) (PostCommentMsg a)
postCommentInit = save {}

postCommentUpdate : PostCommentMsg a -> PostCommentModel -> Update PostCommentModel a (PostCommentMsg a)
postCommentUpdate msg model =
  case msg of
    _ -> save model

postCommentSubscriptions : PostCommentModel -> Sub (PostCommentMsg a)
postCommentSubscriptions model = Sub.none

--

type alias PostListUpdate a = PostListModel -> Update PostListModel a (PostListMsg a)

type PostListMsg a
  = NoPostListMsg

type alias PostListModel =
  {}

postListInit : Update PostListModel (PostListUpdate a) (PostListMsg a)
postListInit = save {}

postListUpdate : PostListMsg a -> PostListModel -> Update PostListModel a (PostListMsg a)
postListUpdate msg model =
  case msg of
    _ -> save model

postListSubscriptions : PostListModel -> Sub (PostListMsg a)
postListSubscriptions model = Sub.none

--

type alias PostCreateUpdate a = PostCreateModel -> Update PostCreateModel a (PostCreateMsg a)

type PostCreateMsg a
  = NoPostCreateMsg

type alias PostCreateModel =
  {}

postCreateInit : Update PostCreateModel (PostCreateUpdate a) (PostCreateMsg a)
postCreateInit = save {}

postCreateUpdate : PostCreateMsg a -> PostCreateModel -> Update PostCreateModel a (PostCreateMsg a)
postCreateUpdate msg model =
  case msg of
    _ -> save model

postCreateSubscriptions : PostCreateModel -> Sub (PostCreateMsg a)
postCreateSubscriptions model = Sub.none

--

type alias Flags = ()

--

type alias PageUpdate a = Page -> Update Page a (PageMsg a)

type PageMsg a
  = HomePageMsg
  | AuthLoginMsg (AuthLoginUpdate a)
  | AuthRegisterMsg (AuthRegisterUpdate a)
  | PostCreateMsg (PostCreateUpdate a)
  | PostShowMsg (PostShowUpdate a)
  | PostCommentMsg (PostCommentUpdate a)

type Page
  = HomePage
  | AboutPage
  | PostCreatePage PostCreateModel
  | PostShowPage PostShowModel
  | PostCommentPage PostCommentModel
  | LoginPage AuthLoginModel
  | RegisterPage AuthRegisterModel

pageInit : Update Page (PageUpdate a) (PageMsg a)
pageInit = save HomePage

loginMsg : AuthLoginMsg a -> PageMsg a
loginMsg = AuthLoginMsg << authLoginUpdate

registerMsg : AuthRegisterMsg a -> PageMsg a
registerMsg = AuthRegisterMsg << authRegisterUpdate

postCreateMsg : PostCreateMsg a -> PageMsg a
postCreateMsg = PostCreateMsg << postCreateUpdate

postShowMsg : PostShowMsg a -> PageMsg a
postShowMsg = PostShowMsg << postShowUpdate

postCommentMsg : PostCommentMsg a -> PageMsg a
postCommentMsg = PostCommentMsg << postCommentUpdate

pageUpdate : PageMsg a -> Page -> Update Page a (PageMsg a)
pageUpdate msg page =
  case ( msg, page ) of
    ( AuthLoginMsg update, LoginPage authLoginModel ) ->
      update authLoginModel
        |> map LoginPage
        |> mapCmd loginMsg
    ( AuthRegisterMsg update, RegisterPage authRegisterModel ) ->
      update authRegisterModel
        |> map RegisterPage
        |> mapCmd registerMsg
    ( PostCreateMsg update, PostCreatePage postCreateModel ) ->
      update postCreateModel
        |> map PostCreatePage
        |> mapCmd postCreateMsg
    ( PostShowMsg update, PostShowPage postShowModel ) ->
      update postShowModel
        |> map PostShowPage
        |> mapCmd postShowMsg
    ( PostCommentMsg update, PostCommentPage postCommentModel ) ->
      update postCommentModel
        |> map PostCommentPage
        |> mapCmd postCommentMsg
    _ ->
      save page

pageSubscriptions : Page -> Sub (Msg a)
pageSubscriptions page =
  case page of
    HomePage ->
      Sub.none
    AboutPage ->
      Sub.none
    PostCreatePage postCreateModel ->
      Sub.map appPostCreateMsg (postCreateSubscriptions postCreateModel)
    PostShowPage postShowModel ->
      Sub.map appPostShowMsg (postShowSubscriptions postShowModel)
    PostCommentPage postCommentModel ->
      Sub.map appPostCommentMsg (postCommentSubscriptions postCommentModel)
    LoginPage authLoginModel ->
      Sub.map appLoginMsg (authLoginSubscriptions authLoginModel)
    RegisterPage authRegisterModel ->
      Sub.map appRegisterMsg (authRegisterSubscriptions authRegisterModel)

--

type alias AppUpdate a = Model -> Update Model a (Msg a)

type Msg a
  = ModelMsg (AppUpdate a)

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : Page }

appInit : Flags -> Url -> Navigation.Key -> Update Model (AppUpdate a) (Msg a)
appInit flags url key =
  let router = routerInit key |> foldEvents
      ui     = uiInit         |> foldEvents
      page   = pageInit       |> foldEvents
   in map3 Model
        (router |> mapCmd routerMsg)
        (ui     |> mapCmd uiMsg)
        (page   |> mapCmd pageMsg)

routerMsg : RouterMsg (AppUpdate a) -> Msg a
routerMsg = message ModelMsg
  { update = routerUpdate { onRouteChange = always save }
  , get = .router
  , set = \model router -> { model | router = router } }

uiMsg : UiMsg (AppUpdate a) -> Msg a
uiMsg = message ModelMsg
  { update = uiUpdate, get = .ui, set = \model ui -> { model | ui = ui } }

pageMsg : PageMsg (AppUpdate a) -> Msg a
pageMsg = message ModelMsg
  { update = pageUpdate, get = .page, set = \model page -> { model | page = page } }

appPostCreateMsg : PostCreateMsg (AppUpdate a) -> Msg a
appPostCreateMsg = pageMsg << PostCreateMsg << postCreateUpdate

appPostShowMsg : PostShowMsg (AppUpdate a) -> Msg a
appPostShowMsg = pageMsg << PostShowMsg << postShowUpdate

appPostCommentMsg : PostCommentMsg (AppUpdate a) -> Msg a
appPostCommentMsg = pageMsg << PostCommentMsg << postCommentUpdate

appLoginMsg : AuthLoginMsg (AppUpdate a) -> Msg a
appLoginMsg = pageMsg << AuthLoginMsg << authLoginUpdate

appRegisterMsg : AuthRegisterMsg (AppUpdate a) -> Msg a
appRegisterMsg = pageMsg << AuthRegisterMsg << authRegisterUpdate

appUpdate : Msg a -> AppUpdate a
appUpdate msg model =
  case msg of
    ModelMsg update ->
      update model

subscriptions : Model -> Sub (Msg a)
subscriptions model =
  Sub.batch
    ( pageSubscriptions model.page ::
      [ Sub.map routerMsg (routerSubscriptions model.router)
      , Sub.map uiMsg (uiSubscriptions model.ui) ] )

view : Model -> Document (Msg a)
view model =
  { title = ""
  , body  = [
      div []
        [ ul []
          [ li [] [ a [ href "/" ] [ text "Home" ] ]
          , li [] [ a [ href "/about" ] [ text "About" ] ]
          , li [] [ a [ href "/login" ] [ text "Login" ] ]
          , li [] [ a [ href "/register" ] [ text "Register" ] ] ]
        , text (Debug.toString model)
        ]
    ]
  }

onUrlChange : Url -> Msg a
onUrlChange url = routerMsg (UrlChange url)

onUrlRequest : UrlRequest -> Msg a
onUrlRequest urlRequest = routerMsg (UrlRequest urlRequest)

main : Program Flags Model (Msg a)
main =
  application
    { init          = appInit
    , update        = appUpdate
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
