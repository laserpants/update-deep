module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todos
import Todos.Form
import Notifications
import Update.Deep exposing (..)
import Update.Deep.Browser as Deep

type Msg
   = TodosMsg Todos.Msg
   | NotificationsMsg Notifications.Msg

type alias State =
  { todos         : Todos.State
  , notifications : Notifications.State }

view : State -> Document Msg
view { todos, notifications } =
  { title = ""
  , body  =
    [ div []
      [ Html.map (TodosMsg << Todos.FormMsg) (Todos.Form.view todos.form)
      , hr [] []
      , Html.map (NotificationsMsg) (Notifications.listView notifications)
      , Html.map TodosMsg (Todos.listView todos) ]
    ]
  }

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

init : () -> Init State Msg
init flags =
  let todos         = Todos.init
      notifications = Notifications.init
   in { todos         = todos.state
      , notifications = notifications.state }
        |> initial
        |> initCmd TodosMsg todos
        |> initCmd NotificationsMsg notifications

insertAsTodosIn : State -> Todos.State -> Update State Msg a
insertAsTodosIn state todos = save { state | todos = todos }

insertAsNotificationsIn : State -> Notifications.State -> Update State Msg a
insertAsNotificationsIn state notifications = save { state | notifications = notifications }

update : Msg -> State -> Update State Msg a
update msg state =
  let notify text = update (NotificationsMsg (Notifications.Add text))
   in case msg of
    TodosMsg todosMsg ->
      let events = { onItemAdded = notify "A new task was added to your list."
                   , onTaskDone  = \item -> notify ("The following task was completed: " ++ item.text) }
       in state.todos
        |> Todos.update events todosMsg
        |> mapCmd TodosMsg
        |> consumeEventsAnd (insertAsTodosIn state)
    NotificationsMsg notificationsMsg ->
      state.notifications
        |> Notifications.update notificationsMsg
        |> mapCmd NotificationsMsg
        |> andThen (insertAsNotificationsIn state)

main : Program () State Msg
main =
  Deep.document
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view }
