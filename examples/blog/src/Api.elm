module Api exposing (..)

import Http exposing (emptyBody)
import Json.Decode as Json exposing (Decoder)
import Update.Deep exposing (..)
import Util exposing (const)

type Msg a
  = Request (Maybe Http.Body)
  | SimpleRequest
  | Response (Result Http.Error a)
  | Reset

type Resource a
  = NotRequested
  | Requested
  | Error Http.Error
  | Available a

type alias Request a = Maybe Http.Body -> Cmd (Msg a)

type alias Api a =
  { resource : Resource a
  , request  : Request a }

setResource : Resource a -> Api a -> Update (Api a) (Msg a) e
setResource resource state = save { state | resource = resource }

type HttpMethod
  = Get
  | Post

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

initRequest : RequestConfig a -> Request a
initRequest { endpoint, method, decoder } body =
  let jsonResponse = Http.expectJson Response
   in case method of
        Get ->
          Http.get { url = endpoint, expect = jsonResponse decoder }
        Post ->
          Http.post
            { url    = endpoint
            , expect = jsonResponse decoder
            , body   = Maybe.withDefault emptyBody body }

init : RequestConfig a -> Init (Api a) (Msg a)
init config =
  { resource = NotRequested
  , request  = initRequest config }
    |> initial

update : { t | onSuccess : a -> Update a c e, onError : Http.Error -> a -> Update a c e } -> Msg b -> Api b -> Update (Api b) (Msg b) (a -> Update a c e)
update events msg state =
  case msg of
    Request maybeBody ->
      state
        |> setResource Requested
        |> andRunCmd (state.request maybeBody)
    SimpleRequest ->
      state
        |> update events (Request Nothing)
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
