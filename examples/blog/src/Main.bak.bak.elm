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

message : (b -> a -> m -> Update m (n -> Update n e c) a) -> { msg : (m -> b -> n -> Update n e c) -> c, set : n -> m -> n } -> a -> c
message update { set, msg } =
  let rec m deep ev model =
        deep
          |> update ev m
          |> mapCmd (msg << rec)
          |> map (set model)
          |> consumeEvents
   in msg << rec


--
--
--

type MegaDeepMsg a
  = MegaDeepHello a

type alias MegaDeepModel =
  { prop : Int }

megaDeepInit : Update MegaDeepModel a c
megaDeepInit =
  save { prop = 5 }

megaDeepUpdate : {} -> MegaDeepMsg a -> MegaDeepModel -> Update MegaDeepModel a (MegaDeepMsg a)
megaDeepUpdate _ msg model =
  case msg of
    _ ->
      save model


megaDeepSubscriptions model = Sub.none

megaDeepView model = div [] []

--

type DeepMsg a
  = DeepHello a
  | SomeDeepMsg
  | DeepMegaDeepMsg (MegaDeepModel -> {} -> DeepModel -> Update DeepModel a (DeepMsg a))

type alias DeepModel =
  { megaDeep : MegaDeepModel }

deepInit : Update DeepModel a (DeepMsg a)
deepInit =
  let megaDeep = megaDeepInit
   in map DeepModel
       (megaDeep |> mapCmd deepMegaDeepMsg)

deepMegaDeepMsg : MegaDeepMsg (DeepModel -> Update DeepModel a (DeepMsg a)) -> DeepMsg a
deepMegaDeepMsg = message megaDeepUpdate
  { set = \model megaDeep -> { model | megaDeep = megaDeep }
  , msg = DeepMegaDeepMsg }

--deepUpdate : { t | onDeepEvent : String -> a, onOtherDeepEvent : String -> a } -> DeepMsg a -> DeepModel -> Update DeepModel a (DeepMsg a)

type alias DeepUpdateEvents a = { onDeepEvent : String -> a, onOtherDeepEvent : String -> a }

deepUpdate : DeepUpdateEvents a -> DeepMsg a -> DeepModel -> Update DeepModel a (DeepMsg a)
deepUpdate { onDeepEvent, onOtherDeepEvent } msg model =
  case msg of
    SomeDeepMsg ->
      save model
    _ ->
      save model

deepSubscriptions model = Sub.none

deepView model = div [] []

--

type alias RouterUpdate a = RouterModel -> Update RouterModel a (RouterMsg a)

type RouterMsg a
  = RouterHello
  | UrlChange Url
  | UrlRequest UrlRequest
  --| RouterDeepMsg (DeepModel -> { onDeepEvent : String -> RouterModel -> Update RouterModel a (RouterMsg a), onOtherDeepEvent : String -> RouterModel -> Update RouterModel a (RouterMsg a) } -> RouterModel -> Update RouterModel a (RouterMsg a))
  | RouterDeepMsg (DeepModel -> DeepUpdateEvents (RouterUpdate a) -> RouterUpdate a)

type alias RouterModel =
  { deep : DeepModel }

routerInit : Update RouterModel a (RouterMsg a)
routerInit = --( { deep = { megaDeep = { prop = 5 } } }, Cmd.none )
  let deep = deepInit
   in consumeEvents <| map RouterModel
        (deep |> mapCmd deepMsg)

deepMsg : DeepMsg (RouterUpdate a) -> RouterMsg a
deepMsg = message deepUpdate
  { set = \model deep -> { model | deep = deep }
  , msg = RouterDeepMsg }

type alias RouterUpdateEvents a = { onRouteChange : Int -> a }

routerUpdate : RouterUpdateEvents a -> RouterMsg a -> RouterModel -> Update RouterModel a (RouterMsg a)
routerUpdate { onRouteChange } msg model =
  case msg of
    RouterHello ->
      save model
        |> andInvokeHandler (onRouteChange 5)
    RouterDeepMsg update ->
      model
        |> update model.deep
             { onDeepEvent      = always save
             , onOtherDeepEvent = always save }
    _ ->
      save model

routerSubscriptions model = Sub.none

routerView model = div [] []

--

type alias Flags = ()

type alias AppUpdate a = Model -> Update Model a (Msg a)

type Msg a
  = RouterMsg (RouterModel -> RouterUpdateEvents (AppUpdate a) -> AppUpdate a)
  | MegaDeepMsg (MegaDeepModel -> {} -> Model -> ( Model, Cmd (Msg a), List a ))
  | NoOp

type alias Model =
  { router   : RouterModel
  , megaDeep : MegaDeepModel }

init : Flags -> Url -> Navigation.Key -> Update Model a (Msg a)
init flags url key = -- ( { router = { deep = { megaDeep = { prop = 5 } } }, megaDeep = { prop = 5 } }, Cmd.none )
  let router   = routerInit
      megaDeep = megaDeepInit
   in map2 Model
        (router   |> mapCmd routerMsg)
        (megaDeep |> mapCmd megaDeepMsg)
          |> consumeEvents

routerMsg : RouterMsg (AppUpdate a) -> Msg a
routerMsg =
  message routerUpdate
    { set = \model router -> { model | router = router }
    , msg = RouterMsg }

megaDeepMsg : MegaDeepMsg (AppUpdate a) -> Msg a
megaDeepMsg =
  message megaDeepUpdate
    { set = \model megaDeep -> { model | megaDeep = megaDeep }
    , msg = MegaDeepMsg }

appUpdate : Msg a -> AppUpdate a
appUpdate msg model =
  case msg of
    RouterMsg update ->
      model
        |> update model.router { onRouteChange = always save }
    MegaDeepMsg update ->
      model
        |> update model.megaDeep {}
    NoOp ->
      model
        |> appUpdate (routerMsg RouterHello)

subscriptions : Model -> Sub (Msg a)
subscriptions model = Sub.none

view : Model -> Document (Msg a)
view model = { title = "", body = [ div [] [] ] }

onUrlChange : Url -> Msg a
onUrlChange url = routerMsg (UrlChange url)

onUrlRequest : UrlRequest -> Msg a
onUrlRequest urlRequest = routerMsg (UrlRequest urlRequest)

main : Program Flags Model (Msg a)
main =
  Browser.application
    { init          = \flags url key -> let (a,b,_) = init flags url key in (a,b)
    , update        = \msg model -> let (a,b,_) = appUpdate msg model in (a,b)
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
