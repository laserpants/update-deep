module Main exposing (Flags, Msg(..), State, init, main, update, view)

import Browser exposing (Document)
import Data.TodoItem exposing (TodoItem)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (..)
import Notifications
import Todos
import Update.Deep exposing (..)
import Update.Deep.Browser as Deep


type alias Flags =
    ()


type Msg
    = TodosMsg Todos.Msg
    | NotificationsMsg Notifications.Msg


type alias State =
    { todos : Todos.State
    , notifications : Notifications.State
    }


inTodos : Wrap State Msg Todos.State Todos.Msg a
inTodos =
    wrapState
        { get = .todos
        , set = \state todos -> { state | todos = todos }
        , msg = TodosMsg
        }


inNotifications : Wrap State Msg Notifications.State Notifications.Msg a
inNotifications =
    wrapState
        { get = .notifications
        , set = \state notifs -> { state | notifications = notifs }
        , msg = NotificationsMsg
        }


init : Flags -> Update State Msg a
init flags =
    save State
        |> andMap (Todos.init TodosMsg)
        |> andMap (Notifications.init NotificationsMsg)


handleItemAdded : TodoItem -> State -> Update State Msg a
handleItemAdded _ =
    inNotifications (Notifications.addNotification "A new task was added to your list.")


handleTaskDone : TodoItem -> State -> Update State Msg a
handleTaskDone item =
    inNotifications (Notifications.addNotification ("The following task was completed: " ++ item.text))


update : Msg -> State -> Update State Msg a
update msg =
    case msg of
        TodosMsg todosMsg ->
            inTodos (Todos.update { onTaskAdded = handleTaskDone, onTaskDone = handleItemAdded } todosMsg)

        NotificationsMsg notificationsMsg ->
            inNotifications (Notifications.update notificationsMsg)


view : State -> Document Msg
view { notifications, todos } =
    { title = ""
    , body =
        [ nav [ class "navbar is-primary" ]
            [ div [ class "navbar-brand" ]
                [ a [ href "#", class "title is-5 navbar-item" ]
                    [ text "trollo" ]
                ]
            ]
        , div [ class "columns is-centered" ]
            [ div [ class "column is-two-thirds" ]
                [ Notifications.view notifications NotificationsMsg
                , Todos.view todos TodosMsg
                ]
            ]
        ]
    }


main : Program () State Msg
main =
    Deep.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
