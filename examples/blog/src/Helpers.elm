module Helpers exposing (..)

import Http exposing (emptyBody)
import Bulma.Modifiers exposing (..)
import Bulma.Components exposing (..)
import Form.Error exposing (ErrorValue(..), Error)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (Validation, oneOf, andThen, succeed, field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Update.Deep.Api as Api

validateStringNonEmpty : Field -> Result (Error e) String
validateStringNonEmpty = 
  [ Validate.string, Validate.emptyString ]
    |> oneOf  
    |> andThen Validate.nonEmpty

validateEmail : Field -> Result (Error e) String
validateEmail = 
  validateStringNonEmpty 
    |> andThen (always Validate.email) 

validationErrorToString : (a -> String) -> ErrorValue a -> String
validationErrorToString customErrorToString error =
  case error of
    Empty ->
      "This field is required"
    InvalidString ->
      "Not a valid string"
    InvalidEmail ->
      "Please enter a valid email address"
    InvalidFormat ->
      "Invalid format"
    InvalidInt ->
      "This value must be an integer"
    InvalidFloat ->
      "This value must be a real number"
    InvalidBool ->
      "Error"
    SmallerIntThan int ->
      "Error"
    GreaterIntThan int ->
      "Error"
    SmallerFloatThan float ->
      "Error"
    GreaterFloatThan float ->
      "Error"
    ShorterStringThan int ->
      "Must be at least " ++ String.fromInt int ++ " characters"
    LongerStringThan int ->
      "Must be no more than " ++ String.fromInt int ++ " characters"
    NotIncludedIn ->
      "Error"
    CustomError e ->
      customErrorToString e

fieldInfo 
   : (a -> String) 
  -> { b | color : Color } 
  -> { e | liveError : Maybe (ErrorValue a), path : c, value : d }
  -> { errorMessage : String
     , hasError : Bool
     , modifiers : { b | color : Color }
     , path : c
     , value : d
     }
fieldInfo custom modifiers { liveError, path, value } =
  case liveError of
    Nothing -> 
      { path = path
      , value = value
      , hasError = False
      , modifiers = modifiers
      , errorMessage = "" 
      }
    Just error ->
      { path = path
      , value = value
      , hasError = True
      , modifiers = { modifiers | color = Danger }
      , errorMessage = validationErrorToString custom error 
      }

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

apiResourceErrorMessage : Api.Resource a -> Html msg
apiResourceErrorMessage resource =
  case resource of
    Api.Error error -> 
      message { messageModifiers | color = Danger } [] 
        [ messageBody [] [ text (error |> httpErrorToString) ] ]
    _ -> text ""

spinner : Html msg
spinner = div [ class "spinner" ] [ div [ class "bounce1" ] [], div [ class "bounce2" ] [], div [ class "bounce3" ] [] ]
