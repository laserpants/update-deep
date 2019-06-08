module Helpers exposing (..)

import Bulma.Modifiers exposing (..)
import Bulma.Components exposing (..)
import Form.Error exposing (ErrorValue(..), Error)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (Validation, succeed, field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep.Api as Api

validateStringNonEmpty : Field -> Result (Error e) String
validateStringNonEmpty = 
  [ Validate.string, Validate.emptyString ]
    |> Validate.oneOf  
    |> Validate.andThen Validate.nonEmpty

validateEmail : Field -> Result (Error e) String
validateEmail = 
  validateStringNonEmpty 
    |> Validate.andThen (always Validate.email) 

errorToString : (a -> String) -> ErrorValue a -> String
errorToString customErrorToString error =
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
      , errorMessage = errorToString custom error 
      }

resourceErrorView : Api.Resource a -> Html msg
resourceErrorView resource =
  case resource of
    Api.Error error -> 
      message { messageModifiers | color = Danger } [] 
        [ messageBody [] [ text (error |> Api.errorToString) ] ]
    _ -> text ""
