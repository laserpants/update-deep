module Page.Home exposing (Msg(..), State, init, subscriptions, update, view)

import Bulma.Elements exposing (..)
import Bulma.Modifiers exposing (..)
import Data.Post as Post exposing (Post)
import Helpers.Api exposing (resourceErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Ui.Page
import Ui exposing (spinner)
import Update.Deep exposing (..)
import Update.Deep.Api as Api


type Msg
    = ApiMsg (Api.Msg (List Post))
    | FetchPosts


type alias State =
    { posts : Api.Model (List Post) }


inPosts : In State (Api.Model (List Post)) msg a
inPosts =
    inState { get = .posts, set = \state posts -> { state | posts = posts } }


init : (Msg -> msg) -> Update State msg a
init toMsg =
    let
        api =
            Api.init
                { endpoint = "/posts"
                , method = Api.HttpGet
                , decoder = Json.field "posts" (Json.list Post.decoder)
                }
    in
    save State
        |> andMap api
        |> mapCmd toMsg


update : Msg -> (Msg -> msg) -> State -> Update State msg a
update msg toMsg =
    case msg of
        ApiMsg apiMsg ->
            inPosts (Api.update { onSuccess = always save, onError = always save } apiMsg (toMsg << ApiMsg))

        FetchPosts ->
            inPosts (Api.sendSimpleRequest (toMsg << ApiMsg))


subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.none


view : State -> (Msg -> msg) -> Html msg
view { posts } toMsg =
    let
        listItem { id, comments, title, body } =
            let
                postUrl =
                    "/posts/" ++ String.fromInt id

                commentsLink =
                    case List.length comments of
                        0 ->
                            text "No comments"

                        n ->
                            a [ href postUrl ]
                                [ text
                                    (if 1 == n then
                                        "1 comment"

                                     else
                                        String.fromInt n ++ " comments"
                                    )
                                ]
            in
            content Standard
                []
                [ h4 [ class "title is-4" ] [ a [ href postUrl ] [ text title ] ]
                , p [] [ text body ]
                , p []
                    [ i [ class "fa fa-comments", style "margin-right" ".5em" ] []
                    , commentsLink
                    ]
                ]

        listView =
            case posts.resource of
                Api.NotRequested ->
                    []

                Api.Requested ->
                    [ spinner ]

                Api.Error error ->
                    [ resourceErrorMessage posts.resource ]

                Api.Available items ->
                    List.map listItem items
    in
    Ui.Page.container "Posts" listView
