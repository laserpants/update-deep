module Todos exposing (..)

import Data.TodoItem exposing (TodoItem)
import Todos.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)

type alias Index = Int

type Msg
  = TodosFormMsg Form.Msg
  | MarkDone Index
  | Delete Index

type alias State = 
  { items : List TodoItem
  , form  : Form.State }

setItems : List TodoItem -> State -> Update State msg a
setItems items state = save { state | items = items } 

addItem : TodoItem -> State -> Update State msg a
addItem item state =
  save { state | items = item :: state.items }

inForm : In State Form.State msg a 
inForm = inState { get = .form, set = \state form -> { state | form = form } }

init : (Msg -> msg) -> Update State msg a
init toMsg =
  save State
    |> andMap (save [])
    |> andMap (Form.init TodosFormMsg)
    |> mapCmd toMsg

update : { onTaskAdded : TodoItem -> a, onTaskDone : TodoItem -> a } -> Msg -> (Msg -> msg) -> State -> Update State msg a
update { onTaskAdded, onTaskDone } msg toMsg =

  let handleSubmit data = 
        let item = { text = data.text }
         in addItem item 
              >> andInvokeHandler (onTaskAdded item)

      removeItem ix shouldNotify state =
        state |> case List.drop ix state.items of
          [] ->
            save
          (item :: rest) ->
            setItems (List.take ix state.items ++ rest) 
              >> andThenIf (always shouldNotify) (invokeHandler <| onTaskDone item)

   in 
       case msg of
         TodosFormMsg formMsg ->
           inForm (Form.update { onSubmit = handleSubmit } formMsg)
         MarkDone ix ->
           removeItem ix True
         Delete ix ->
           removeItem ix False

view : State -> (Msg -> msg) -> Html msg
view { items, form } toMsg =
  let indexed = List.indexedMap Tuple.pair
      row (ix, todo) =
        tr [] 
          [ td []
            [ text todo.text ]
          , td []
            [ span [ class "icon has-text-success" ] 
              [ i [ class "fa fa-check-square" ] [] ]
            , a [ onClick (toMsg <| MarkDone ix), href "#" ]
              [ text "Done" ]
            , span [ style "margin-left" ".5em"
                   , class "icon has-text-danger" ] 
              [ i [ class "fa fa-trash" ] [] ]
            , a [ onClick (toMsg <| Delete ix), href "#" ]
              [ text "Delete" ] ]
          ]

   in
       div [ style "margin" "1em" ] 
         [ Form.view form (toMsg << TodosFormMsg) 
         , h3 [ class "title is-3" ] [ text "Tasks" ]
         , if List.isEmpty items 
               then p [] [ text "You have no tasks" ] 
               else table [ class "table is-bordered is-fullwidth" ] (List.map row (indexed items)) ]
