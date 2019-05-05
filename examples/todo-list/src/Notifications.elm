module Notifications exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Task
import List exposing (isEmpty, length, reverse, take)
import Update.Deep exposing (..)

type Msg
  = Add String
  | DismissOne
  | DismissAll

type alias State =
  { list : List String }

addNotification : String -> State -> Update State Msg a
addNotification text state = save { state | list = text :: state.list }

dismissOne : State -> Update State Msg a
dismissOne state = save { state | list = List.take (List.length state.list - 1) state.list }

dismissAll : State -> Update State Msg a
dismissAll state = save { state | list = [] }

dismissAfterSeconds : Int -> State -> Update State Msg a
dismissAfterSeconds n =
  let secs = toFloat n
   in runCmd (Task.perform (always DismissOne) (Process.sleep (secs * 1000)))

update : Msg -> State -> Update State Msg a
update msg state =
  let setTimeoutIf cond = if cond then dismissAfterSeconds 6 else save
   in case msg of
    Add text ->
      state
        |> addNotification text
        |> andThen (setTimeoutIf (isEmpty state.list))
    DismissOne ->
      state
        |> dismissOne
        |> andThen (setTimeoutIf (length state.list > 1))
    DismissAll ->
      state
        |> dismissAll

init : Init State Msg
init = initial { list = [] }

view : State -> Html Msg
view { list } =
  let len = length list
      item text = Html.text text
   in div []
       ([ text (if len > 1 then "(" ++ String.fromInt len ++ ")" else "")
        , text " " ] ++ List.map item (list |> reverse |> take 1))
