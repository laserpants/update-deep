module App.Posts exposing (..)

import Api
import App.Config exposing (..)
import App.Posts.Create.Page as CreatePage
import App.Posts.Item.Page as ItemPage
import App.Posts.List.Page as ListPage
import Update.Deep exposing (..)

type Msg
  = ListMsg ListPage.Msg
  | CreateMsg CreatePage.Msg
  | ItemMsg ItemPage.Msg
  | SetPage Int
  | FetchAll

type alias State =
  { listPage   : ListPage.State
  , createPage : CreatePage.State
  , itemPage   : ItemPage.State }

init : Config -> Init State Msg
init config =
  let listPage   = ListPage.init config
      createPage = CreatePage.init config
      itemPage   = ItemPage.init config
   in { listPage   = listPage.state
      , createPage = createPage.state
      , itemPage   = itemPage.state }
        |> initial
        |> initCmd ListMsg listPage
        |> initCmd CreateMsg createPage
        |> initCmd ItemMsg itemPage

update : Msg -> State -> Update State Msg a
update msg state =
  case msg of
    ListMsg listPageMsg ->
      state.listPage
        |> ListPage.update listPageMsg
        |> andThen (\page -> save { state | listPage = page })
        |> mapCmd ListMsg
    CreateMsg createPageMsg ->
      state.createPage
        |> CreatePage.update { onPostAdded = always save } createPageMsg
        |> andThen (\page -> save { state | createPage = page })
        |> mapCmd CreateMsg
        |> consumeEvents
    ItemMsg itemPageMsg ->
      state.itemPage
        |> ItemPage.update itemPageMsg
        |> andThen (\page -> save { state | itemPage = page })
        |> mapCmd ItemMsg
    SetPage id ->
      state
        |> update (ItemMsg (ItemPage.SetPost id))
    FetchAll ->
      state
        |> update (ListMsg ListPage.FetchAll)

subscriptions : State -> Sub Msg
subscriptions { listPage, createPage, itemPage } =
  Sub.batch
    [ Sub.map ListMsg (ListPage.subscriptions listPage)
    , Sub.map CreateMsg (CreatePage.subscriptions createPage)
    , Sub.map ItemMsg (ItemPage.subscriptions itemPage) ]
