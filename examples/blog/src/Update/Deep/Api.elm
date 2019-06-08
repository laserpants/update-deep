module Update.Deep.Api exposing (..)

import Json.Decode as Json
import Http exposing (emptyBody)
import Update.Deep exposing (..)

type Msg a
  = Response (Result Http.Error a)
  | Reset

type Resource a
  = NotRequested
  | Requested
  | Error Http.Error
  | Available a

type alias Request a = String -> Maybe Http.Body -> Cmd (Msg a)

type alias Model a =
  { resource : Resource a
  , request  : Request a }

setResource : Resource a -> Model a -> Update (Model a) msg b
setResource resource state = save { state | resource = resource }

type HttpMethod
  = HttpGet
  | HttpPost
  | HttpPut

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

init : RequestConfig a -> Update (Model a) msg b
init { endpoint, method, decoder } =
  let expect = Http.expectJson Response decoder
      request suffix body =
        Http.request
          { method  = case method of
                        HttpGet  -> "GET"
                        HttpPost -> "POST"
                        HttpPut  -> "PUT"
          , headers = []
          , url     = endpoint ++ suffix
          , expect  = expect
          , body    = Maybe.withDefault emptyBody body
          , timeout = Nothing
          , tracker = Nothing }
   in save { resource = NotRequested, request = request }

sendRequest : String -> Maybe Http.Body -> (Msg a -> msg) -> Model a -> Update (Model a) msg b
sendRequest url maybeBody toMsg model =
  model
    |> setResource Requested
    |> andAddCmd (model.request url maybeBody)
    |> mapCmd toMsg

sendSimpleRequest : (Msg a -> msg) -> Model a -> Update (Model a) msg b
sendSimpleRequest = sendRequest "" Nothing

resetResource : Model a -> Update (Model a) msg b
resetResource = setResource NotRequested

type alias ApiEventHandlers a b = { onSuccess : a -> b, onError : Http.Error -> b }

update : ApiEventHandlers a b -> Msg a -> (Msg a -> msg) -> Model a -> Update (Model a) msg b
update { onSuccess, onError } msg toMsg =
  case msg of
    Response (Ok resource) ->
      setResource (Available resource)
        >> andInvokeHandler (onSuccess resource)
    Response (Err error) ->
      setResource (Error error)
        >> andInvokeHandler (onError error)
    Reset ->
      resetResource

errorToString : Http.Error -> String
errorToString error =
  case error of
    Http.BadStatus 401 ->
      "Authentication failed."
    Http.BadStatus 500 ->
      "Application error (500 Internal Server Error)"
    Http.BadStatus 501 ->
      "This feature is not implemented"
    _ ->
      "Something went wrong!"
