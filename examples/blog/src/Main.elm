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

save model = ( model, Cmd.none, [] )

runCmd cmd model = ( model, cmd, [] )

mapCmd f ( model, cmd, events ) = ( model, Cmd.map f cmd, events )

invokeHandler handler model = ( model , Cmd.none, [ handler ] )

ap ( f, cmda, e ) ( model, cmdb, e2 ) = ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

map f ( model, cmd, events ) = ( f model, cmd, events )

map2 f = ap << map f

map3 f x = ap << map2 f x

map4 f x y = ap << map3 f x y

join ( ( model, cmda, e ), cmdb, e2 ) = ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

andThen f = join << map f

kleisli f g = andThen f << g

andRunCmd = andThen << runCmd

andInvokeHandler = andThen << invokeHandler

consumeEvents ( model, cmd, events ) = List.foldr andThen ( model, cmd, [] ) events

--type alias X = 
--  { msg : (b -> c -> ( d, Cmd msg, List e )) -> msg
--  , set  : c -> f -> d
--  , update  : a -> b -> ( b, Cmd a, List (d -> ( d, Cmd msg, List e )) ) 
--  }

--someFun : X -> a -> msg

--(RouterModel -> Model -> ( Model, Cmd (Msg a), List a ))

--type alias X a b c e f =
--  { msg : (f -> b -> ( b, Cmd c, List e )) -> c
--  , set  : b -> f -> b
--  , update  : a -> f -> ( f, Cmd a, List (b -> ( b, Cmd c, List e )) ) }
--
--someFun : X a b c d e -> a -> c
someFun update { set, msg } = 
  let rec msg_ deep model = 
        deep
          |> update msg_
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

megaDeepInit = ( { prop = 5 }, Cmd.none )

megaDeepUpdate msg model = 
  case msg of
    _ ->
      save model
      

megaDeepSubscriptions model = Sub.none

megaDeepView model = div [] []

--

type DeepMsg a
  = DeepHello a
  | SomeDeepMsg

type alias DeepModel =
  { megaDeep : MegaDeepModel }

deepInit = ( { megaDeep = { prop = 5 } }, Cmd.none )

deepUpdate : { onDeepEvent : String -> a, onOtherDeepEvent : String -> a } -> DeepMsg a -> DeepModel -> ( DeepModel, Cmd (DeepMsg a), List a )
deepUpdate { onDeepEvent, onOtherDeepEvent } msg model = 
  case msg of
    SomeDeepMsg ->
      save model
    _ ->
      save model

deepSubscriptions model = Sub.none

deepView model = div [] []

--

type RouterMsg a
  = RouterHello
  | UrlChange Url
  | UrlRequest UrlRequest
  | RouterDeepMsg (DeepModel -> RouterModel -> ( RouterModel, Cmd (RouterMsg a), List a ))

type alias RouterModel =
  { deep : DeepModel }

routerInit = ( { deep = { megaDeep = { prop = 5 } } }, Cmd.none )

routerUpdate : { t | onRouteChange : number -> a } -> RouterMsg a -> RouterModel -> ( RouterModel, Cmd (RouterMsg a), List a )
routerUpdate { onRouteChange } msg model = 
  case msg of
    RouterHello ->
      save model
        |> andInvokeHandler (onRouteChange 5)
    RouterDeepMsg update ->
      update model.deep model
    _ ->
      save model

deepMsg : DeepMsg (RouterModel -> ( RouterModel, Cmd (RouterMsg a), List a )) -> RouterMsg a
deepMsg = someFun 
  (deepUpdate { onDeepEvent = always save, onOtherDeepEvent = always save })
    { set = \model deep -> { model | deep = deep }
    , msg = RouterDeepMsg }

routerSubscriptions model = Sub.none

routerView model = div [] []

--

type alias Flags = ()

type Msg a
  = RouterMsg (RouterModel -> Model -> ( Model, Cmd (Msg a), List a ))
  | MegaDeepMsg (MegaDeepModel -> Model -> ( Model, Cmd (Msg a), List a ))
  | NoOp

type alias Model =
  { router   : RouterModel
  , megaDeep : MegaDeepModel }

init flags url key = ( { router = { deep = { megaDeep = { prop = 5 } } }, megaDeep = { prop = 5 } }, Cmd.none )

routerMsg : RouterMsg (Model -> ( Model, Cmd (Msg a), List a )) -> Msg a
routerMsg = someFun 
  (routerUpdate { onRouteChange = \route -> save })
    { set = \model router -> { model | router = router }
    , msg = RouterMsg }

appUpdate : Msg a -> Model -> ( Model, Cmd (Msg a), List a )
appUpdate msg model = 
  case msg of
    RouterMsg update ->
      model
        |> update model.router
    MegaDeepMsg update ->
      model
        |> update model.megaDeep
    NoOp ->
      model
        |> appUpdate (routerMsg RouterHello)

subscriptions model = Sub.none

view model = { title = "", body = [ div [] [] ] }

onUrlChange url = routerMsg (UrlChange url)

onUrlRequest urlRequest = routerMsg (UrlRequest urlRequest)

main : Program Flags Model (Msg a)
main =
  Browser.application
    { init          = init
    , update        = \msg model -> let (a,b,_) = appUpdate msg model in (a,b)
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
