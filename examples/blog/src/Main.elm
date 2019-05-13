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

consumeEvents : Update m (m -> Update m a c) c -> Update m a c
consumeEvents ( model, cmd, events ) = List.foldr andThen ( model, cmd, [] ) events

message cons get update set =
  let fun msg model = 
        model
          |> get 
          |> update msg 
          |> mapCmd (cons << fun)
          |> map (set model) 
          |> consumeEvents 
   in cons << fun

-- --message : (Model -> c) -> (b -> c -> Update m (Model -> Update Model a (Msg a)) b) -> (Model -> m -> Model) -> b -> Msg a
-- message get update set =
--   let fun msg model = 
--         model
--           |> get 
--           |> update msg 
--           |> mapCmd (ModelMsg << fun)
--           |> map (set model) 
--           |> consumeEvents 
--    in ModelMsg << fun

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

type RouterMsg a
  = UrlChange Url
  | UrlRequest UrlRequest

type alias RouterModel =
  { route : Maybe Route
  , key   : Navigation.Key }

setRoute : Maybe Route -> RouterModel -> Update RouterModel a (RouterMsg a)
setRoute route model = save { model | route = route }

routerInit : Navigation.Key -> Update RouterModel a (RouterMsg a)
routerInit key = 
  save 
    { route = Nothing
    , key   = key }

routerUpdate : { onRouteChange : Maybe Route -> a } -> RouterMsg a -> RouterModel -> Update RouterModel a (RouterMsg a)
routerUpdate { onRouteChange } msg model =
  case msg of
    UrlChange url ->
      let route = fromUrl url
       in model
        |> setRoute route
        |> andInvokeHandler (onRouteChange route)
    UrlRequest urlRequest ->
      save model

routerSubscriptions : RouterModel -> Sub (RouterMsg a)
routerSubscriptions model = Sub.none

--

type UiMsg a
  = NoUiMsg a

type alias UiModel =
  {}

uiInit : Update UiModel a (UiMsg a)
uiInit = save {}

uiUpdate : UiMsg a -> UiModel -> Update UiModel a (UiMsg a)
uiUpdate msg model =
  save model

uiSubscriptions : UiModel -> Sub (UiMsg a)
uiSubscriptions model = Sub.none

--

type PageMsg a
  = NoPageMsg a

type alias PageModel =
  {}

pageInit : Update PageModel a (PageMsg a)
pageInit = save {}

pageUpdate : PageMsg a -> PageModel -> Update PageModel a (PageMsg a)
pageUpdate msg model = 
  save model

pageSubscriptions : PageModel -> Sub (PageMsg a)
pageSubscriptions model = Sub.none

--

type alias Flags = ()

--

type alias AppUpdate a = Model -> Update Model a (Msg a)

type Msg a
  = ModelMsg (Model -> Update Model a (Msg a))
  | Nope

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : PageModel }

appInit : Flags -> Url -> Navigation.Key -> Update Model a (Msg a)
appInit flags url key = Debug.todo ""

routerMsg : RouterMsg (AppUpdate a) -> Msg a
routerMsg = message 
  ModelMsg 
  .router 
  (routerUpdate { onRouteChange = always save }) 
  (\model router -> { model | router = router }) 

uiMsg : UiMsg (AppUpdate a) -> Msg a
uiMsg = message 
  ModelMsg 
  .ui 
  uiUpdate 
  (\model ui -> { model | ui = ui }) 

pageMsg : PageMsg (AppUpdate a) -> Msg a
pageMsg = message 
  ModelMsg 
  .page 
  pageUpdate 
  (\model page -> { model | page = page }) 

appUpdate : Msg a -> AppUpdate a
appUpdate msg model =
  case msg of
    ModelMsg update ->
      update model 
    _ ->
      save model
--    RouterMsg update ->
--      model
--        |> update model.router { onRouteChange = always save }
--    UiMsg update ->
--      model
--        |> update model.ui {}
--    PageMsg update ->
--      model
--        |> update model.page {}

subscriptions : Model -> Sub (Msg a)
subscriptions model =
  Sub.batch
    [ Sub.map routerMsg (routerSubscriptions model.router)
    , Sub.map uiMsg (uiSubscriptions model.ui)
    , Sub.map pageMsg (pageSubscriptions model.page) ]

view : Model -> Document (Msg a)
view model = { title = "", body = [ div [] [] ] }

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
