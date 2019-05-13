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

message : ((n -> Update n b c) -> c) -> { update : a -> m -> Update m (n -> Update n b c) a, get : n -> m, set : n -> m -> n } -> a -> c
message cons { update, get, set } =
  let rec msg model =
        model
          |> get
          |> update msg
          |> mapCmd (cons << rec)
          |> map (set model)
          |> foldEvents
   in cons << rec

--
--
--

type Route
  = Home
  | About
  | NewPost
  | Post Int
  | CommentPost Int
  | Login
  | Register

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home        (Parser.top)
    , Parser.map About       (Parser.s "about")
    , Parser.map NewPost     (Parser.s "posts" </> Parser.s "new")
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
  | RouterModelMsg (RouterUpdate a)

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
    RouterModelMsg update ->
      update model

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

type alias PageUpdate a = PageModel -> Update PageModel a (PageMsg a)

type PageMsg a
  = NoPageMsg

type alias PageModel =
  {}

pageInit : Update PageModel (PageUpdate a) (PageMsg a)
pageInit = save {}

pageUpdate : PageMsg a -> PageModel -> Update PageModel a (PageMsg a)
pageUpdate msg model = save model

pageSubscriptions : PageModel -> Sub (PageMsg a)
pageSubscriptions model = Sub.none

--

type alias Flags = ()

--

type alias AppUpdate a = Model -> Update Model a (Msg a)

type Msg a
  = ModelMsg (AppUpdate a)

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : PageModel }

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
  { update = uiUpdate
  , get = .ui
  , set = \model ui -> { model | ui = ui } }

pageMsg : PageMsg (AppUpdate a) -> Msg a
pageMsg = message ModelMsg
  { update = pageUpdate
  , get = .page
  , set = \model page -> { model | page = page } }

appUpdate : Msg a -> AppUpdate a
appUpdate msg model =
  case msg of
    ModelMsg update ->
      update model 

subscriptions : Model -> Sub (Msg a)
subscriptions model =
  Sub.batch
    [ Sub.map routerMsg (routerSubscriptions model.router)
    , Sub.map uiMsg (uiSubscriptions model.ui)
    , Sub.map pageMsg (pageSubscriptions model.page) ]

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
  Browser.application
    { init          = \flags url key -> let (a,b,_) = appInit flags url key in (a,b)
    , update        = \msg model -> let (a,b,_) = appUpdate msg model in (a,b)
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
