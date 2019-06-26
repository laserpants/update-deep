module Notifications exposing (Msg(..), State, addNotification, dismissAfterSeconds, enqueuNotification, incrementCounter, init, removeWithId, resetQueue, update, view)

import Data.Notification exposing (Notification, NotificationId(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Task
import Update.Deep exposing (..)


type Msg
    = DismissOne NotificationId
    | DismissAll


type alias State =
    { queue : List Notification
    , counter : Int
    }


enqueuNotification : Notification -> State -> Update State msg a
enqueuNotification notification state =
    save { state | queue = notification :: state.queue }


removeWithId : NotificationId -> State -> Update State msg a
removeWithId id_ state =
    save { state | queue = List.filter (\{ id } -> id /= id_) state.queue }


resetQueue : State -> Update State msg a
resetQueue state =
    save { state | queue = [] }


incrementCounter : State -> Update State msg a
incrementCounter state =
    save { state | counter = 1 + state.counter }


init : (Msg -> msg) -> Update State msg a
init toMsg =
    save State
        |> andMap (save [])
        |> andMap (save 1)
        |> mapCmd toMsg


dismissAfterSeconds : Int -> Notification -> State -> Update State Msg a
dismissAfterSeconds n { id } =
    let
        secs =
            toFloat n
    in
    addCmd (Task.perform (always <| DismissOne id) (Process.sleep (secs * 1000)))


addNotification : String -> State -> Update State Msg a
addNotification text state =
    let
        notification =
            { id = Id state.counter, text = text }
    in
    state
        |> enqueuNotification notification
        |> andThen (notification |> dismissAfterSeconds 10)
        |> andThen incrementCounter


update : Msg -> State -> Update State Msg a
update msg =
    case msg of
        DismissOne notificationId ->
            removeWithId notificationId

        DismissAll ->
            resetQueue


view : State -> (Msg -> msg) -> Html msg
view { queue } toMsg =
    let
        item { id, text } =
            div [ class "notification" ]
                [ button [ onClick (toMsg <| DismissOne id), class "delete" ] []
                , Html.text text
                ]
    in
    div [ style "margin" "1em" ]
        [ div [] (List.map item queue)
        , if List.isEmpty queue then
            text ""

          else
            div [ class "has-text-centered", style "margin-top" ".5em" ] [ a [ href "#", onClick (toMsg DismissAll) ] [ text "Dismiss all notifications" ] ]
        ]
