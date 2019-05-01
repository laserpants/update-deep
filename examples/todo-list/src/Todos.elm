module Todos exposing (..)

import Data.TodoItem exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todos.Form as Form
import Update.Deep exposing (..)
import Util exposing (const, uncurry)

type alias Index = Int

type Msg
  = FormMsg Form.Msg
  | AddItem TodoItem
  | DeleteItem Index
  | TaskDone Index

type alias State =
  { list : List TodoItem
  , form : Form.State }

pushItem : TodoItem -> State -> Update State Msg a
pushItem item state = save { state | list = item :: state.list }

removeItem : { t | onDelete : TodoItem -> a } -> Index -> State -> Update State Msg a
removeItem { onDelete } ix ({ list } as state) =
  case List.drop ix list of
    [] ->
      save state
    (item :: rest) ->
      { state | list = List.take ix list ++ rest }
        |> save
        |> andInvoke (onDelete item)

update : { t | onItemAdded : a -> Update a c e, onTaskDone : TodoItem -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
update ({ onItemAdded, onTaskDone } as events) msg state =
  case msg of
    FormMsg formMsg ->
      state.form
        |> Form.update { onSubmit = update events << AddItem } formMsg
        |> mapCmd FormMsg
        |> consumeEventsAnd (\form -> save { state | form = form})
    AddItem item ->
      state
        |> pushItem item
        |> andInvoke onItemAdded
    TaskDone ix ->
      state
        |> removeItem { onDelete = onTaskDone } ix
    DeleteItem ix ->
      state
        |> removeItem { onDelete = const save } ix

init : Init State Msg
init =
  let form = Form.init
   in { list = [], form = form.state }
        |> initial
        |> initCmd FormMsg form

listView : State -> Html Msg
listView { list } =
  let indexed = List.indexedMap Tuple.pair
      item ix todo =
        li [] [ a [ href "#", onClick (TaskDone ix) ] [ text "Done" ]
              , text " "
              , a [ href "#", onClick (DeleteItem ix) ] [ text "Delete" ]
              , text " "
              , text (String.fromInt ix)
              , text ": "
              , text todo.text ]
   in ul [] (List.map (uncurry item) (indexed list))
