module Helpers.Api exposing (..)

import Bulma.Modifiers exposing (..)
import Bulma.Components exposing (..)
import Update.Deep.Api as Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (emptyBody)

httpErrorToString : Http.Error -> String
httpErrorToString error =
  case error of
    Http.BadStatus 401 ->
      "Authentication failed."
    Http.BadStatus 500 ->
      "Application error (500 Internal Server Error)"
    Http.BadStatus 501 ->
      "This feature is not implemented"
    _ ->
      "Something went wrong!"

resourceErrorMessage : Api.Resource a -> Html msg
resourceErrorMessage resource =
  case resource of
    Api.Error error -> 
      message { messageModifiers | color = Danger } [] 
        [ messageBody [] [ text (error |> httpErrorToString) ] ]
    _ -> text ""
