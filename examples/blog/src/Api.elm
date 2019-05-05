module Api exposing (..)

import Http exposing (emptyBody)
import Json.Decode as Json exposing (Decoder, Value)
import Update.Deep exposing (..)
import Util exposing (const)

type Msg a
  = Request (Maybe Http.Body)
  | RequestResource String (Maybe Http.Body)
  | Response (Result Http.Error a)
  | Reset

type Resource a
  = NotRequested
  | Requested
  | Error Http.Error
  | Available a

type alias Request a = String -> Maybe Http.Body -> Cmd (Msg a)

type alias Api a =
  { resource : Resource a
  , request  : Request a }

setResource : Resource a -> Api a -> Update (Api a) (Msg a) e
setResource resource state = save { state | resource = resource }

type HttpMethod
  = HttpGet
  | HttpPost

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

initRequest : RequestConfig a -> Request a
initRequest { endpoint, method, decoder } suffix body =
  let expect = Http.expectJson Response decoder
      url = endpoint ++ suffix
   in case method of
        HttpGet ->
          Http.get
            { url    = url
            , expect = expect }
        HttpPost ->
          Http.post
            { url    = url
            , expect = expect
            , body   = Maybe.withDefault emptyBody body }

init : RequestConfig a -> Init (Api a) (Msg a)
init config =
  { resource = NotRequested
  , request  = initRequest config } |> initial

defaultHandlers : { onSuccess : a -> Update a c e, onError : Http.Error -> a -> Update a c e }
defaultHandlers = { onSuccess = save, onError = const save }

update : { t | onSuccess : a -> Update a c e, onError : Http.Error -> a -> Update a c e } -> Msg b -> Api b -> Update (Api b) (Msg b) (a -> Update a c e)
update events msg state =
  case msg of
    Request maybeBody ->
      state
        |> update events (RequestResource "" maybeBody)
    RequestResource endpoint maybeBody ->
      state
        |> setResource Requested
        |> andRunCmd (state.request endpoint maybeBody)
    Response (Ok response) ->
      state
        |> setResource (Available response)
        |> andInvoke events.onSuccess
    Response (Err error) ->
      state
        |> setResource (Error error)
        |> andInvoke (events.onError error)
    Reset ->
      state
        |> setResource NotRequested

jsonRequest : Value -> Msg a
jsonRequest = Request << Just << Http.jsonBody
