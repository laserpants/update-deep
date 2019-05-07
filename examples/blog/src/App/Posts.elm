module App.Posts exposing (..)

import Api
import App.Config exposing (..)
import App.Posts.Create as Create
import App.Posts.Item as Item
import App.Posts.List as List
import Data.Post exposing (Post)
import Update.Deep exposing (..)
import Browser.Navigation as Navigation

type Msg
  = ListMsg List.Msg
  | CreateMsg Create.Msg
  | ItemMsg Item.Msg
  | SetPost Int
  | FetchAll

type alias State =
  { list   : List.State
  , create : Create.State
  , item   : Item.State }

init : Config -> Init State Msg
init config =
  let list   = List.init config
      create = Create.init config
      item   = Item.init config
   in { list   = list.state
      , create = create.state
      , item   = item.state }
        |> initial
        |> initCmd ListMsg list
        |> initCmd CreateMsg create
        |> initCmd ItemMsg item

onPostAdded : { redirect : String -> a -> Update a c e } -> Post -> State -> Update State Msg (a -> Update a c e)
onPostAdded events post state =
  state
    |> update events (ListMsg (List.FetchAll True))
    |> andThen (invoke (events.redirect "/"))

update : { redirect : String -> a -> Update a c e } -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  case msg of
    ListMsg listMsg ->
      state.list
        |> List.update listMsg
        |> andThen (\page -> save { state | list = page })
        |> mapCmd ListMsg
    CreateMsg createMsg ->
      state.create
        |> Create.update { onPostAdded = onPostAdded events } createMsg
        |> andThen (\page -> save { state | create = page })
        |> mapCmd CreateMsg
        |> consumeEvents
    ItemMsg itemMsg ->
      state.item
        |> Item.update itemMsg
        |> andThen (\page -> save { state | item = page })
        |> mapCmd ItemMsg
    SetPost id ->
      state
        |> update events (ItemMsg (Item.SetPost id))
    FetchAll ->
      state
        |> update events (ListMsg (List.FetchAll False))

subscriptions : State -> Sub Msg
subscriptions { list, create, item } =
  Sub.batch
    [ Sub.map ListMsg (List.subscriptions list)
    , Sub.map CreateMsg (Create.subscriptions create)
    , Sub.map ItemMsg (Item.subscriptions item) ]
