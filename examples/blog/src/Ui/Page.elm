module Ui.Page exposing (container)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


container : String -> List (Html msg) -> Html msg
container title html =
    div [ class "columns is-centered", style "margin" "1.5em" ]
        [ div [ class "column is-two-thirds" ]
            [ h3 [ class "title is-3" ] [ text title ]
            , p [ class "content" ] html
            ]
        ]
