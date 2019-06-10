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
    , pathname : String
    }


setRoute : Maybe route -> State route -> Update (State route) msg a
setRoute route state =
    save { state | route = route }


init : (Url -> Maybe route) -> String -> Navigation.Key -> (Msg -> msg) -> Update (State route) msg a
init fromUrl pathname key toMsg =
    save State
        |> andMap (save Nothing)
        |> andMap (save key)
        |> andMap (save fromUrl)
        |> andMap (save pathname)


redirect : String -> State route -> Update (State route) msg a
redirect href state =
    state
        |> addCmd (Navigation.replaceUrl state.key (state.pathname ++ href))


update : { onRouteChange : Url -> Maybe route -> a } -> Msg -> State route -> Update (State route) msg a
update { onRouteChange } msg state =
    let
        stripPath url =
            { url | path = String.dropLeft (String.length state.pathname) url.path }

        insertPath url =
            { url | path = state.pathname ++ url.path }
    in
    case msg of
        UrlChange url ->
            let
                route =
                    state.fromUrl (stripPath url)
            in
            state
                |> setRoute route
                |> andInvokeHandler (onRouteChange url route)

        UrlRequest (Browser.Internal url) ->
            state
                |> addCmd (Navigation.pushUrl state.key (Url.toString (insertPath url)))

        UrlRequest (Browser.External "") ->
            state
                |> save

        UrlRequest (Browser.External href) ->
            state
                |> addCmd (Navigation.load href)
