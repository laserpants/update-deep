module Form.Register exposing (..)

import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Register.Custom as Custom exposing (Error(..)) 
import Form.Error exposing (ErrorValue(..), Error)
import Form.Validate as Validate exposing (Validation, succeed, field)
import Helpers exposing (..)
import Json.Decode as Json
import Json.Encode as Encode

type alias Fields =
  { name : String
  , email : String
  , username: String
  , phoneNumber : String
  , password : String
  , passwordConfirmation : String 
  , agreeWithTerms : Bool 
  }

validatePassword : Field -> Result (Error e) String
validatePassword = 
  validateStringNonEmpty
    |> Validate.andThen (Validate.minLength 8)

validatePasswordConfirmation : Field -> Result (Error Custom.Error) String
validatePasswordConfirmation = 

  let 
      match password confirmation = 
        if password == confirmation
            then Validate.succeed confirmation
            else Validate.fail (Validate.customError PasswordConfirmationMismatch)
   in 
       [ Validate.string, Validate.emptyString ]
         |> Validate.oneOf
         |> field "password"
         |> Validate.andThen (\value -> 
              validateStringNonEmpty
                |> Validate.andThen (match value)
                |> field "passwordConfirmation")

validateChecked : Field -> Result (Error Custom.Error) Bool
validateChecked = 

  let 
      mustBeChecked checked =
        if checked
            then Validate.succeed True
            else Validate.fail (Validate.customError MustAgreeWithTerms)
   in
      Validate.bool 
        |> Validate.andThen mustBeChecked

validate : Validation Custom.Error Fields
validate =
  succeed Fields
    |> Validate.andMap (field "name" validateStringNonEmpty)
    |> Validate.andMap (field "email" validateEmail)
    |> Validate.andMap (field "username" validateStringNonEmpty)
    |> Validate.andMap (field "phoneNumber" validateStringNonEmpty)
    |> Validate.andMap (field "password" validatePassword)
    |> Validate.andMap validatePasswordConfirmation
    |> Validate.andMap (field "agreeWithTerms" validateChecked)

toJson : Fields -> Json.Value
toJson { name, email, username, phoneNumber, password, agreeWithTerms } = 
  Encode.object 
    [ ( "name" , Encode.string name )
    , ( "email" , Encode.string email )
    , ( "username" , Encode.string username )
    , ( "phoneNumber" , Encode.string phoneNumber )
    , ( "password" , Encode.string password )
    , ( "agreeWithTerms" , Encode.bool agreeWithTerms )
    ]
