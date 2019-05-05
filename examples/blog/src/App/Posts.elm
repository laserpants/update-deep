module App.Posts exposing (..)

import App.Comments.Page as CommentsPage
import App.Config exposing (..)
import App.Posts.Create.Page as CreatePage
import App.Posts.Item.Page as ItemPage
import App.Posts.List.Page as ListPage
import Update.Deep exposing (..)

type Msg
  = ListMsg ListPage.Msg
  | CreateMsg CreatePage.Msg
  | ItemMsg ItemPage.Msg
  | CommentsMsg CommentsPage.Msg

type alias State =
  { listPage     : ListPage.State
  , createPage   : CreatePage.State
  , itemPage     : ItemPage.State
  , commentsPage : CommentsPage.State }

init : Config -> Init State Msg
init config =
  let listPage     = ListPage.init config
      createPage   = CreatePage.init config
      itemPage     = ItemPage.init config
      commentsPage = CommentsPage.init config
   in { listPage     = listPage.state
      , createPage   = createPage.state
      , itemPage     = itemPage.state
      , commentsPage = commentsPage.state }
        |> initial
        |> initCmd ListMsg listPage
        |> initCmd CreateMsg createPage
        |> initCmd ItemMsg itemPage
        |> initCmd CommentsMsg commentsPage

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
        |> CreatePage.update createPageMsg
        |> andThen (\page -> save { state | createPage = page })
        |> mapCmd CreateMsg
    ItemMsg itemPageMsg ->
      state.itemPage
        |> ItemPage.update itemPageMsg
        |> andThen (\page -> save { state | itemPage = page })
        |> mapCmd ItemMsg
    CommentsMsg commentsPageMsg ->
      state.commentsPage
        |> CommentsPage.update commentsPageMsg
        |> andThen (\page -> save { state | commentsPage = page })
        |> mapCmd CommentsMsg

subscriptions : State -> Sub Msg
subscriptions { listPage, createPage, itemPage, commentsPage } =
  Sub.batch
    [ Sub.map ListMsg (ListPage.subscriptions listPage)
    , Sub.map CreateMsg (CreatePage.subscriptions createPage)
    , Sub.map ItemMsg (ItemPage.subscriptions itemPage)
    , Sub.map CommentsMsg (CommentsPage.subscriptions commentsPage) ]

--formView : State -> Html Msg
--formView { form } = Html.map FormMsg (FormState.view form)
--
--postView : Post -> Html Msg
--postView { id, title, body } =
--  let postUrl = "posts/" ++ String.fromInt id
--   in div []
--    [ h2 [] [ text title ]
--    , p [] [ text body ]
--    , a [ href postUrl ] [ text "Show" ]
--    , text " | "
--    , a [ href (postUrl ++ "/comments/new") ]
--        [ text "Comment" ] ]
--
--listView : State -> Html Msg
--listView { collection } =
--  case collection.resource of
--    NotRequested ->
--      div [] [ text "Not requested"
--             , button [ onClick FetchPosts ] [ text "Fetch" ] ]
--    Requested ->
--      div [] [ text "Requested..." ]
--    Error error ->
--      div [] [ text "Error" ]
--    Available posts ->
--      div [] (List.map postView posts)
--
--itemView : State -> Html Msg
--itemView { postFetch } =
--  case postFetch.resource of
--    NotRequested ->
--      div [] [ text "Not requested" ]
--    Requested ->
--      div [] [ text "Fetching..." ]
--    Error (Http.BadStatus 404) ->
--      div [] [ text "That post was not found" ]
--    Error error ->
--      div [] [ text "Error", text (Debug.toString error) ]
--    Available post ->
--      div []
--        [ div [] [ text "Post item" ]
--        , div [] [ a [ href ("/posts/" ++ String.fromInt post.id ++ "/comments/new") ] [ text "Comment" ] ] ]
