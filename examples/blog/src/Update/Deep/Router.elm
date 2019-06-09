module Update.Deep.Router exposing (Msg(..), State, init, redirect, setRoute, update)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Update.Deep exposing (..)
import Url exposing (Url)


type Msg
    = UrlChange Url
    | UrlRequest UrlRequest


type alias State route =
    { route : Maybe route
    , key : Navigation.Key
    , fromUrl : Url -> Maybe route
    }


setRoute : Maybe route -> State route -> Update (State route) msg a
setRoute route state =
    save { state | route = route }


init : (Url -> Maybe route) -> Navigation.Key -> (Msg -> msg) -> Update (State route) msg a
init fromUrl key toMsg =
    save State
        |> andMap (save Nothing)
        |> andMap (save key)
        |> andMap (save fromUrl)


redirect : String -> State route -> Update (State route) msg a
redirect href state =
    state
        |> addCmd (Navigation.replaceUrl state.key href)


update : { onRouteChange : Url -> Maybe route -> a } -> Msg -> State route -> Update (State route) msg a
update { onRouteChange } msg state =
    case msg of
        UrlChange url ->
            let
                route =
                    state.fromUrl url
            in
            state
                |> setRoute route
                |> andInvokeHandler (onRouteChange url route)

        UrlRequest (Browser.Internal url) ->
            state
                |> addCmd (Navigation.pushUrl state.key (Url.toString url))

        UrlRequest (Browser.External "") ->
            state
                |> save

        UrlRequest (Browser.External href) ->
            state
                |> addCmd (Navigation.load href)
