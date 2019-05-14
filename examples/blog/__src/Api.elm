module Api exposing (..)

import Http exposing (emptyBody)
import Json.Decode as Json exposing (Decoder, Value)
import Update.Deep exposing (..)

type Msg a
  = Request (Maybe Http.Body)
  | RequestUrl String (Maybe Http.Body)
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

defaultHandlers : { onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e }
defaultHandlers = { onSuccess = always save
                  , onError   = always save }

update : { t | onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e } -> Msg b -> Api b -> Update (Api b) (Msg b) (a -> Update a c e)
update events msg state =
  case msg of
    Request maybeBody ->
      state
        |> update events (RequestUrl "" maybeBody)
    RequestUrl endpoint maybeBody ->
      state
        |> setResource Requested
        |> andRunCmd (state.request endpoint maybeBody)
    Response (Ok resource) ->
      state
        |> setResource (Available resource)
        |> andInvoke (events.onSuccess resource)
    Response (Err error) ->
      state
        |> setResource (Error error)
        |> andInvoke (events.onError error)
    Reset ->
      state
        |> setResource NotRequested

jsonRequest : Value -> Msg a
jsonRequest = Request << Just << Http.jsonBody
