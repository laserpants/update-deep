module Todos exposing (..)

import Data.TodoItem exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todos.Form as Form
import Update.Deep exposing (..)

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

type alias EventHandlers t a c e =
  { t | onItemAdded : a -> Update a c e
      , onTaskDone  : TodoItem -> a -> Update a c e }

update : EventHandlers t a c e  -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  case msg of
    FormMsg formMsg ->
      state.form
        |> Form.update { onSubmit = \todo -> update events (AddItem todo) } formMsg
        |> mapCmd FormMsg
        |> andThen (\form -> save { state | form = form})
        |> consumeEvents
    AddItem item ->
      state
        |> pushItem item
        |> andInvoke events.onItemAdded
    TaskDone ix ->
      state
        |> removeItem { onDelete = events.onTaskDone } ix
    DeleteItem ix ->
      state
        |> removeItem { onDelete = always save } ix

init : Init State Msg
init =
  let form = Form.init
   in { list = [], form = form.state }
        |> initial
        |> initCmd FormMsg form

view : State -> Html Msg
view { list } =
  let indexed = List.indexedMap Tuple.pair
      item (ix, todo) =
        li [] [ a [ href "#", onClick (TaskDone ix) ] [ text "Done" ]
              , text " | "
              , a [ href "#", onClick (DeleteItem ix) ] [ text "Delete" ]
              , text " | "
              , text todo.text ]
   in div []
        [ if not (List.isEmpty list) then h3 [] [ text "Todos" ] else text ""
        , ul [] (List.map item (indexed list)) ]
