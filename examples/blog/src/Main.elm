module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..), Error)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, succeed, field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (emptyBody)
import Json.Decode as Json
import Json.Encode exposing (object)
import Maybe.Extra as Maybe
import Ports
import Update.Deep exposing (..)
import Update.Deep.Browser as Deep
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, parse, oneOf, (</>))

--

-- custom errors
-- redirect after login

--

type alias WebSocketUsernameAvailableResponsePayload =
  { username : String
  , available : Bool 
  }

webSocketUsernameAvailableResponseDecoder : Json.Decoder WebSocketUsernameAvailableResponsePayload
webSocketUsernameAvailableResponseDecoder =
  Json.map2 WebSocketUsernameAvailableResponsePayload
    (Json.field "username" Json.string)
    (Json.field "available" Json.bool)

type WebSocketMessage
  = WebSocketUsernameAvailableResponse WebSocketUsernameAvailableResponsePayload

websocketMessageDecoder : Json.Decoder WebSocketMessage
websocketMessageDecoder =
  let payloadDecoder type_ =
        case type_ of
          "username_available_response" ->
            Json.map WebSocketUsernameAvailableResponse webSocketUsernameAvailableResponseDecoder
          _ ->
            Json.fail "Unrecognized message type"
   in Json.field "type" Json.string |> Json.andThen payloadDecoder

--

type alias Post =
  { id    : Int
  , title : String
  , body  : String }

postDecoder : Json.Decoder Post
postDecoder =
  Json.map3 Post
    (Json.field "id"    Json.int)
    (Json.field "title" Json.string)
    (Json.field "body"  Json.string)

--

validateStringNonEmpty : Field -> Result (Error e) String
validateStringNonEmpty = 
  [ Validate.string, Validate.emptyString ]
    |> Validate.oneOf  
    |> Validate.andThen Validate.nonEmpty

--

type alias FormModel a b =
  { form : Form a b 
  , disabled : Bool 
  , validation : Validation a b }

formInsertAsFormIn : FormModel a b -> Form a b -> Update (FormModel a b) msg c
formInsertAsFormIn model form = save { model | form = form }

formSetDisabled : Bool -> FormModel a b -> Update (FormModel a b) msg c
formSetDisabled disabled state = save { state | disabled = disabled }

formInit : List ( String, Field ) -> Validation a b -> Update (FormModel a b) msg c
formInit fields validation =
  Update.Deep.map3 FormModel
    (Form.initial fields validation |> save)
    (save False)
    (save validation)

formReset : List ( String, Field ) -> FormModel a b -> Update (FormModel a b) msg c
formReset fields model = 
  model.form
    |> Form.update model.validation (Form.Reset fields)
    |> formInsertAsFormIn model
    |> andThen (formSetDisabled False)

formUpdate : { onSubmit : b -> c } -> Form.Msg -> FormModel a b -> Update (FormModel a b) msg c
formUpdate { onSubmit } msg model = 
  case ( msg, Form.getOutput model.form ) of
    ( Form.Submit, Just form ) ->
      model
        |> formSetDisabled True
        |> andThen (invokeHandler (onSubmit form))
    _ ->
      model.form
        |> Form.update model.validation msg
        |> formInsertAsFormIn model

--

type ApiMsg a
  = Response (Result Http.Error a)
  | Reset

type ApiResource a
  = NotRequested
  | Requested
  | Error Http.Error
  | Available a

type alias Request a = String -> Maybe Http.Body -> Cmd (ApiMsg a)

type alias ApiModel a =
  { resource : ApiResource a
  , request  : Request a }

setResource : ApiResource a -> ApiModel a -> Update (ApiModel a) msg b
setResource resource state = save { state | resource = resource }

type HttpMethod
  = HttpGet
  | HttpPost
  | HttpPut

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

apiInit : RequestConfig a -> Update (ApiModel a) msg b
apiInit { endpoint, method, decoder } =
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

apiSendRequest : String -> Maybe Http.Body -> (ApiMsg a -> msg) -> ApiModel a -> Update (ApiModel a) msg b
apiSendRequest url maybeBody toMsg model =
  model
    |> setResource Requested
    |> andAddCmd (model.request url maybeBody)
    |> mapCmd toMsg

apiResetResource : ApiModel a -> Update (ApiModel a) msg b
apiResetResource = setResource NotRequested

type alias ApiEventHandlers a b = { onSuccess : a -> b, onError : Http.Error -> b }

apiUpdate : ApiEventHandlers a b -> ApiMsg a -> (ApiMsg a -> msg) -> ApiModel a -> Update (ApiModel a) msg b
apiUpdate { onSuccess, onError } msg toMsg =
  case msg of
    Response (Ok resource) ->
      setResource (Available resource)
        >> andInvokeHandler (onSuccess resource)
    Response (Err error) ->
      setResource (Error error)
        >> andInvokeHandler (onError error)
    Reset ->
      apiResetResource

--

errorToString : ErrorValue a -> String
errorToString error =
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
      Debug.toString e -- TODO

--

type HomePageMsg 
  = HomePageApiMsg (ApiMsg (List Post))
  | FetchPosts

type alias HomePageState =
  { posts : ApiModel (List Post) }

homePageInPosts : In HomePageState (ApiModel (List Post)) msg a
homePageInPosts =
    inState { get = .posts, set = \state posts -> { state | posts = posts } }

homePageInit : (HomePageMsg -> msg) -> Update HomePageState msg a
homePageInit toMsg = 
  let 
      api = apiInit { endpoint = "/posts"
                    , method   = HttpGet
                    , decoder  = Json.field "posts" (Json.list postDecoder) }

   in 
      save HomePageState
        |> andMap api
        |> mapCmd toMsg

homePageUpdate : HomePageMsg -> (HomePageMsg -> msg) -> HomePageState -> Update HomePageState msg a
homePageUpdate msg toMsg state = 
  case msg of
    HomePageApiMsg apiMsg ->
      state
        |> homePageInPosts (apiUpdate { onSuccess = always save, onError = always save } apiMsg (toMsg << HomePageApiMsg))
    FetchPosts ->
      state
        |> homePageInPosts (apiSendRequest "" Nothing (toMsg << HomePageApiMsg))

homePageSubscriptions : HomePageState -> (HomePageMsg -> msg) -> Sub msg
homePageSubscriptions state toMsg = Sub.none

homePagePostsList : ApiModel (List Post) -> Html msg
homePagePostsList { resource } = 

  let listItem post =
        div [] 
          [ h3 [] [ text post.title ] 
          , p [] [ text post.body ] ]

   in 
       case resource of
         NotRequested ->
           div [] []
         Requested ->
           div [] [ text "Loading" ]
         Error error ->
           div [] [ text "error" ]
         Available posts ->
           div [] (List.map listItem posts)

homePageView : HomePageState -> (HomePageMsg -> msg) -> Html msg
homePageView state toMsg = 
  div [] 
    [ h2 [] [ text "x" ]
    , homePagePostsList state.posts
    , button [ onClick (toMsg FetchPosts) ] [ text "fetch" ]
    ]

--

type alias NewPostForm =
  { title : String
  , body : String 
  }

newPostFormValidate : Validation () NewPostForm
newPostFormValidate =
  succeed NewPostForm
    |> Validate.andMap (field "title" validateStringNonEmpty)
    |> Validate.andMap (field "body" validateStringNonEmpty)

newPostFormToJson : NewPostForm -> Json.Value
newPostFormToJson { title, body } = 
  object 
    [ ( "title" , Json.Encode.string title )
    , ( "body" , Json.Encode.string body )
    ]

--

type NewPostPageMsg 
  = NewPostPageApiMsg (ApiMsg Post)
  | NewPostFormMsg Form.Msg

type alias NewPostPageState =
  { api : ApiModel Post
  , formModel : FormModel () NewPostForm }

newPostPageInApi : In NewPostPageState (ApiModel Post) msg a
newPostPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

newPostPageInForm : In NewPostPageState (FormModel () NewPostForm) msg a
newPostPageInForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

newPostPageInit : (NewPostPageMsg -> msg) -> Update NewPostPageState msg a
newPostPageInit toMsg = 
  let
      api = apiInit { endpoint = "/posts"
                    , method   = HttpPost
                    , decoder  = Json.field "post" postDecoder }

      form = formInit [] newPostFormValidate
   in 
      save NewPostPageState
        |> andMap api
        |> andMap form
        |> mapCmd toMsg

newPostPageUpdate : { onPostAdded : Post -> a } -> NewPostPageMsg -> (NewPostPageMsg -> msg) -> NewPostPageState -> Update NewPostPageState msg a
newPostPageUpdate { onPostAdded } msg toMsg state = 

  let
      handleSubmit form = 
          let json = form |> newPostFormToJson |> Http.jsonBody 
           in newPostPageInApi (apiSendRequest "" (Just json) (toMsg << NewPostPageApiMsg))
   in
      case msg of
        NewPostPageApiMsg apiMsg ->
           state
             |> newPostPageInApi (apiUpdate { onSuccess = invokeHandler << onPostAdded, onError = always save } apiMsg (toMsg << NewPostPageApiMsg))
        NewPostFormMsg formMsg ->
          state
            |> newPostPageInForm (formUpdate { onSubmit = handleSubmit } formMsg)

newPostPageSubscriptions : NewPostPageState -> (NewPostPageMsg -> msg) -> Sub msg
newPostPageSubscriptions state toMsg = Sub.none

newPostPageFormView : FormModel () NewPostForm -> (Form.Msg -> msg) -> Html msg
newPostPageFormView { form, disabled } toMsg =

  let title = form |> Form.getFieldAsString "title"
      body = form |> Form.getFieldAsString "body"

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack (always "") errorToString

   in

      [ fieldset [ Html.Attributes.disabled disabled ]
        [ div [] 
          [ input 
            [ onFocus (Form.Focus title.path)
            , onBlur (Form.Blur title.path)
            , onInput (String >> Form.Input title.path Form.Text)
            , value (Maybe.withDefault "" title.value)  
            ] [] 
          ]
        , div [] [ Html.text (errorMessage title) ]
        , div [] 
          [ textarea 
            [ onFocus (Form.Focus body.path)
            , onBlur (Form.Blur body.path)
            , onInput (String >> Form.Input body.path Form.Text)
            , value (Maybe.withDefault "" body.value) 
            ] []
          ]
        , div [] [ Html.text (errorMessage body) ]
        , div []
          [ button [ type_ "submit" ] [ text (if disabled then "Please wait" else "Log in") ] 
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

newPostPageView : NewPostPageState -> (NewPostPageMsg -> msg) -> Html msg
newPostPageView { formModel } toMsg = 
  div [] [ newPostPageFormView formModel (toMsg << NewPostFormMsg) ]

--

type ShowPostPageMsg 
  = NoShowPostPageMsg

type alias ShowPostPageState =
  {}

showPostPageInit : (ShowPostPageMsg -> msg) -> Update ShowPostPageState msg a
showPostPageInit toMsg = 
  save ShowPostPageState
    |> mapCmd toMsg

showPostPageUpdate : ShowPostPageMsg -> ShowPostPageState -> Update ShowPostPageState msg a
showPostPageUpdate msg state = Debug.todo ""

showPostPageSubscriptions : ShowPostPageState -> (ShowPostPageMsg -> msg) -> Sub msg
showPostPageSubscriptions state toMsg = Sub.none

showPostPageView : ShowPostPageState -> (ShowPostPageMsg -> msg) -> Html msg
showPostPageView state toMsg = 
  div [] [ text "show post" ]

--

type alias LoginForm =
  { username : String
  , password : String 
  }

loginFormValidate : Validation () LoginForm
loginFormValidate =
  succeed LoginForm
    |> Validate.andMap (field "username" validateStringNonEmpty)
    |> Validate.andMap (field "password" validateStringNonEmpty)

loginFormToJson : LoginForm -> Json.Value
loginFormToJson { username, password } = 
  object 
    [ ( "username" , Json.Encode.string username )
    , ( "password" , Json.Encode.string password )
    ]

--

type LoginPageMsg 
  = LoginFormMsg Form.Msg
  | LoginPageApiMsg (ApiMsg Session)

type alias LoginPageState =
  { api : ApiModel Session
  , formModel : FormModel () LoginForm 
  }

loginPageInApi : In LoginPageState (ApiModel Session) msg a
loginPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

loginPageInForm : In LoginPageState (FormModel () LoginForm) msg a
loginPageInForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

loginPageInit : (LoginPageMsg -> msg) -> Update LoginPageState msg a
loginPageInit toMsg = 
  let 
      api = apiInit { endpoint = "/auth/login"
                    , method   = HttpPost
                    , decoder  = Json.field "session" sessionDecoder }

      form = formInit [] loginFormValidate
   in 
      save LoginPageState
        |> andMap api
        |> andMap form
        |> mapCmd toMsg

loginPageUpdate : { onAuthResponse : Maybe Session -> a } -> LoginPageMsg -> (LoginPageMsg -> msg) -> LoginPageState -> Update LoginPageState msg a
loginPageUpdate { onAuthResponse } msg toMsg state =
  let 
      handleSubmit form = 
          let json = form |> loginFormToJson |> Http.jsonBody 
           in loginPageInApi (apiSendRequest "" (Just json) (toMsg << LoginPageApiMsg))

      handleApiResponse maybeSession = 
        loginPageInForm (formReset []) 
          >> andInvokeHandler (onAuthResponse maybeSession)

   in case msg of
     LoginPageApiMsg apiMsg -> 
       state
         |> loginPageInApi (apiUpdate { onSuccess = handleApiResponse << Just, onError = handleApiResponse Nothing |> always } apiMsg (toMsg << LoginPageApiMsg))
     LoginFormMsg formMsg ->
       state
         |> loginPageInForm (formUpdate { onSubmit = handleSubmit } formMsg)

loginPageSubscriptions : LoginPageState -> (LoginPageMsg -> msg) -> Sub msg
loginPageSubscriptions state toMsg = Sub.none

loginPageFormView : FormModel () LoginForm -> (Form.Msg -> msg) -> Html msg
loginPageFormView { form, disabled } toMsg =

  let username = form |> Form.getFieldAsString "username"
      password = form |> Form.getFieldAsString "password"

      inputField attributes field = input 
        ( [ onFocus (Form.Focus field.path)
          , onBlur (Form.Blur field.path)
          , onInput (String >> Form.Input field.path Form.Text)
          , value (Maybe.withDefault "" field.value)
          ] ++ attributes ) []

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack (always "") errorToString

   in

      [ fieldset [ Html.Attributes.disabled disabled ]
        [ div [] [ inputField [] username ]
        , div [] [ Html.text (errorMessage username) ]
        , div [] [ inputField [ type_ "password" ] password ]
        , div [] [ Html.text (errorMessage password) ]
        , div []
          [ button [ type_ "submit" ] [ text (if disabled then "Please wait" else "Log in") ] 
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

loginPageView : LoginPageState -> (LoginPageMsg -> msg) -> Html msg
loginPageView { formModel } toMsg = 
  div [] [ loginPageFormView formModel (toMsg << LoginFormMsg) ]

--

type alias RegisterForm =
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
    |> field "password"

validatePasswordConfirmation : Field -> Result (Error e) String
validatePasswordConfirmation = 

  let match password confirmation = 
        if password == confirmation
            then Validate.succeed confirmation
            else Validate.fail (Form.Error.value InvalidBool)

   in 
       [ field "password" Validate.string
       , field "password" Validate.emptyString 
       ]
         |> Validate.oneOf
         |> Validate.andThen (\value -> 
             validateStringNonEmpty
               |> Validate.andThen (match value)
               |> field "passwordConfirmation")

validateAgreeWithTerms : Field -> Result (Error e) Bool
validateAgreeWithTerms = 

  let 
      mustBeChecked checked =
        if checked
            then Validate.succeed True
            else Validate.fail (Form.Error.value InvalidBool)
   in
      Validate.bool 
        |> Validate.andThen mustBeChecked
        |> field "agreeWithTerms" 

validateEmail : Field -> Result (Error e) String
validateEmail = 
  Validate.sequence [ field "email" Validate.email, field "email" validateStringNonEmpty ]
    |> Validate.map (Maybe.withDefault "" << List.head)

registerFormValidate : Validation () RegisterForm
registerFormValidate =
  succeed RegisterForm
    |> Validate.andMap (field "name" validateStringNonEmpty)
    |> Validate.andMap validateEmail 
    |> Validate.andMap (field "username" validateStringNonEmpty)
    |> Validate.andMap (field "phoneNumber" validateStringNonEmpty)
    |> Validate.andMap validatePassword
    |> Validate.andMap validatePasswordConfirmation
    |> Validate.andMap validateAgreeWithTerms

registerFormToJson : RegisterForm -> Json.Value
registerFormToJson { name, email, username, phoneNumber, password, agreeWithTerms } = 
  object 
    [ ( "name" , Json.Encode.string name )
    , ( "email" , Json.Encode.string email )
    , ( "username" , Json.Encode.string username )
    , ( "phoneNumber" , Json.Encode.string phoneNumber )
    , ( "password" , Json.Encode.string password )
    , ( "agreeWithTerms" , Json.Encode.bool agreeWithTerms )
    ]

--

type RegisterPageMsg 
  = RegisterPageApiMsg (ApiMsg User)
  | RegisterFormMsg Form.Msg
  | RegisterPageWebsocketMsg String

type UsernameStatus
  = UsernameBlank
  | UsernameAvailable Bool
  | Unknown

type alias RegisterPageState =
  { api : ApiModel User 
  , formModel : FormModel () RegisterForm 
  , usernames : Dict String Bool 
  , usernameStatus : UsernameStatus
  }

saveUsernameStatus : String -> Bool -> RegisterPageState -> Update RegisterPageState msg a
saveUsernameStatus username available state = save { state | usernames = Dict.insert username available state.usernames }

registerPageInApi : In RegisterPageState (ApiModel User) msg a
registerPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

registerPageInForm : In RegisterPageState (FormModel () RegisterForm) msg a
registerPageInForm =
    inState { get = .formModel, set = \state form -> { state | formModel = form } }

setUsernameStatus : UsernameStatus -> RegisterPageState -> Update RegisterPageState msg a
setUsernameStatus status state = save { state | usernameStatus = status }

registerPageInit : (RegisterPageMsg -> msg) -> Update RegisterPageState msg a
registerPageInit toMsg = 
  let 
      api = apiInit { endpoint = "/auth/register"
                    , method   = HttpPost
                    , decoder  = Json.field "user" userDecoder }

      form = formInit [] registerFormValidate

   in 
      save RegisterPageState
        |> andMap api
        |> andMap form
        |> andMap (save Dict.empty)
        |> andMap (save UsernameBlank)
        |> mapCmd toMsg

websocketUsernameAvailableQuery : String -> Json.Value
websocketUsernameAvailableQuery username =
  Json.Encode.object
    [ ( "type"  , Json.Encode.string "username_available_query" )
    , ( "username" , Json.Encode.string username ) 
    ]

checkIfUsernameAvailable : String -> RegisterPageState -> Update RegisterPageState msg a
checkIfUsernameAvailable username ({ usernames } as state) = 
  if String.isEmpty username
      then 
        state
          |> setUsernameStatus UsernameBlank
      else 
        case Dict.get username usernames of
          Just isAvailable ->
            state
              |> setUsernameStatus (UsernameAvailable isAvailable)
          Nothing ->
            state
              |> setUsernameStatus Unknown
              |> andAddCmd (Ports.websocketOut (Json.Encode.encode 0 (websocketUsernameAvailableQuery username)))

usernameFieldSpy : Form.Msg -> RegisterPageState -> Update RegisterPageState msg a
usernameFieldSpy formMsg =
  case formMsg of
    Form.Input "username" Form.Text (String username) ->
      checkIfUsernameAvailable username 
    _ ->
      save 

registerPageUpdate : RegisterPageMsg -> (RegisterPageMsg -> msg) -> RegisterPageState -> Update RegisterPageState msg a
registerPageUpdate msg toMsg state = 

  let
      handleSubmit form = 
          let json = form |> registerFormToJson |> Http.jsonBody 
           in registerPageInApi (apiSendRequest "" (Just json) (toMsg << RegisterPageApiMsg))

   in 
      case msg of
        RegisterPageApiMsg apiMsg ->
          state
            |> registerPageInApi (apiUpdate { onSuccess = always save, onError = always save } apiMsg (toMsg << RegisterPageApiMsg))
        RegisterFormMsg formMsg ->
          state
            |> registerPageInForm (formUpdate { onSubmit = handleSubmit } formMsg)
            |> andThen (usernameFieldSpy formMsg)
        RegisterPageWebsocketMsg wsMsg ->
          case Json.decodeString websocketMessageDecoder wsMsg of
            Ok (WebSocketUsernameAvailableResponse { username, available }) ->
              let 
                  usernameField = Form.getFieldAsString "username" state.formModel.form
               in 
                  state
                    |> saveUsernameStatus username available
                    |> andThen (checkIfUsernameAvailable <| Maybe.withDefault "" usernameField.value)
            _ ->
              save state

registerPageSubscriptions : RegisterPageState -> (RegisterPageMsg -> msg) -> Sub msg
registerPageSubscriptions state toMsg = Ports.websocketIn (toMsg << RegisterPageWebsocketMsg)

registerPageFormView : FormModel () RegisterForm -> UsernameStatus -> (Form.Msg -> msg) -> Html msg
registerPageFormView { form, disabled } usernameStatus toMsg =

  let 
      name                 = form |> Form.getFieldAsString "name"
      email                = form |> Form.getFieldAsString "email"
      username             = form |> Form.getFieldAsString "username"
      phoneNumber          = form |> Form.getFieldAsString "phoneNumber"
      password             = form |> Form.getFieldAsString "password"
      passwordConfirmation = form |> Form.getFieldAsString "passwordConfirmation"
      agreeWithTerms       = form |> Form.getFieldAsBool   "agreeWithTerms"

      inputField attributes field = input 
        ( [ onFocus (Form.Focus field.path)
          , onBlur (Form.Blur field.path)
          , onInput (String >> Form.Input field.path Form.Text)
          , value (Maybe.withDefault "" field.value)
          ] ++ attributes ) []

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack (always "") errorToString

      usernameStatusText =
        case usernameStatus of
          UsernameBlank ->
            text "[blank]"
          UsernameAvailable isAvailable ->
            text (if isAvailable then "Available" else "Not available")
          Unknown ->
            text "..."

   in

      [ fieldset [ Html.Attributes.disabled disabled ]
        [ div [] [ text "Name" ] 
        , div [] [ inputField [] name ] 
        , div [] [ Html.text (errorMessage name) ]
        , div [] [ text "Email" ] 
        , div [] [ inputField [] email ]
        , div [] [ Html.text (errorMessage email) ]
        , div [] [ text "Username" ] 
        , div [] [ inputField [] username, usernameStatusText ]
        , div [] [ Html.text (errorMessage username) ]
        , div [] [ text "Phone number" ] 
        , div [] [ inputField [] phoneNumber ]
        , div [] [ Html.text (errorMessage phoneNumber) ]
        , div [] [ text "Password" ] 
        , div [] [ inputField [ type_ "password" ] password ]
        , div [] [ Html.text (errorMessage password) ]
        , div [] [ text "Password confirmation" ] 
        , div [] [ inputField [ type_ "password" ] passwordConfirmation ]
        , div [] [ Html.text (errorMessage passwordConfirmation) ]
        , div [] 
          [ input 
            [ type_ "checkbox"
            , onFocus (Form.Focus agreeWithTerms.path)
            , onBlur (Form.Blur agreeWithTerms.path)
            , onCheck (Bool >> Form.Input agreeWithTerms.path Form.Checkbox)
            , checked (Maybe.withDefault False agreeWithTerms.value)
            ] 
            []
          ]
        , div [] [ Html.text (agreeWithTerms.liveError |> Maybe.unpack (always "") errorToString) ]
        , div []
          [ button [ type_ "submit" ] [ text (if disabled then "Please wait" else "Submit") ] 
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

registerPageView : RegisterPageState -> (RegisterPageMsg -> msg) -> Html msg
registerPageView { api, formModel, usernameStatus } toMsg = 
  case api.resource of
    Available response ->
      div [] [ text "Thanks!" ]
    Error error ->
      div [] [ text "error" ]
    _ ->
      div [] [ registerPageFormView formModel usernameStatus (toMsg << RegisterFormMsg) ]

--

type Route
  = Home
  | Login
  | Logout
  | Register
  | About
  | NewPost
  | ShowPost Int

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home (Parser.top)
    , Parser.map Login (Parser.s "login")
    , Parser.map Logout (Parser.s "logout")
    , Parser.map Register (Parser.s "register")
    , Parser.map About (Parser.s "about")
    , Parser.map NewPost (Parser.s "posts" </> Parser.s "new")
    , Parser.map ShowPost (Parser.s "posts" </> Parser.int)
    ]
    
fromUrl : Url -> Maybe Route
fromUrl = parse parser

--

type RouterMsg 
 = UrlChange Url
 | UrlRequest UrlRequest

type alias RouterState =
 { route : Maybe Route
 , key   : Navigation.Key
 }

setRoute : Maybe Route -> RouterState -> Update RouterState msg a
setRoute route state = save { state | route = route }

routerInit : Navigation.Key -> (RouterMsg -> msg) -> Update RouterState msg a
routerInit key toMsg = 
  save RouterState
    |> andMap (save Nothing)
    |> andMap (save key)

routerRedirect : String -> RouterState -> Update RouterState msg a
routerRedirect href state = 
  state
    |> addCmd (Navigation.replaceUrl state.key href)

routerUpdate : { onRouteChange : Maybe Route -> a } -> RouterMsg -> RouterState -> Update RouterState msg a
routerUpdate { onRouteChange } msg state = 
  case msg of
    UrlChange url ->
      let route = fromUrl url
       in state
        |> setRoute route
        |> andInvokeHandler (onRouteChange route)
    UrlRequest (Browser.Internal url) ->
      state
        |> addCmd (Navigation.pushUrl state.key (Url.toString url))
    UrlRequest (Browser.External href) ->
      state
        |> addCmd (Navigation.load href)

routerSubscriptions : RouterState -> (RouterMsg -> msg) -> Sub msg
routerSubscriptions state toMsg = Sub.none

--

type PageMsg
  = HomePageMsg HomePageMsg
  | NewPostPageMsg NewPostPageMsg
  | ShowPostPageMsg ShowPostPageMsg
  | LoginPageMsg LoginPageMsg
  | RegisterPageMsg RegisterPageMsg

type Page
  = HomePage HomePageState
  | NewPostPage NewPostPageState
  | ShowPostPage ShowPostPageState
  | LoginPage LoginPageState
  | RegisterPage RegisterPageState
  | AboutPage
  | NotFoundPage

pageUpdate : { onAuthResponse : Maybe Session -> a, onPostAdded : Post -> a } -> PageMsg -> (PageMsg -> msg) -> Page -> Update Page msg a
pageUpdate { onAuthResponse, onPostAdded } msg toMsg page = 
  case page of
    HomePage homePageState ->
      case msg of
        HomePageMsg homePageMsg ->
          homePageState
            |> homePageUpdate homePageMsg (toMsg << HomePageMsg)
            |> Update.Deep.map HomePage 
        _ ->
          save page
    NewPostPage newPostPageState ->
      case msg of
        NewPostPageMsg newPostPageMsg ->
          newPostPageState
            |> newPostPageUpdate { onPostAdded = onPostAdded } newPostPageMsg (toMsg << NewPostPageMsg)
            |> Update.Deep.map NewPostPage 
        _ ->
          save page
    ShowPostPage showPostPageState ->
      case msg of
        ShowPostPageMsg showPostPageMsg ->
          showPostPageState
            |> showPostPageUpdate showPostPageMsg 
            |> Update.Deep.map ShowPostPage 
        _ ->
          save page
    LoginPage loginPageState ->
      case msg of
        LoginPageMsg loginPageMsg ->
          loginPageState
            |> loginPageUpdate { onAuthResponse = onAuthResponse } loginPageMsg (toMsg << LoginPageMsg)
            |> Update.Deep.map LoginPage 
        _ -> 
          save page
    RegisterPage registerPageState ->
      case msg of
        RegisterPageMsg registerPageMsg ->
          registerPageState
            |> registerPageUpdate registerPageMsg (toMsg << RegisterPageMsg)
            |> Update.Deep.map RegisterPage 
        _ ->
          save page
    AboutPage ->
      save page
    NotFoundPage ->
      save page

pageSubscriptions : Page -> (PageMsg -> msg) -> Sub msg
pageSubscriptions page toMsg = 
  case page of
    HomePage homePageState ->
      homePageSubscriptions homePageState (toMsg << HomePageMsg)
    NewPostPage newPostPageState ->
      newPostPageSubscriptions newPostPageState (toMsg << NewPostPageMsg)
    ShowPostPage showPostPageState ->
      showPostPageSubscriptions showPostPageState (toMsg << ShowPostPageMsg)
    LoginPage loginPageState ->
      loginPageSubscriptions loginPageState (toMsg << LoginPageMsg)
    RegisterPage registerPageState ->
      registerPageSubscriptions registerPageState (toMsg << RegisterPageMsg)
    AboutPage ->
      Sub.none
    NotFoundPage ->
      Sub.none

pageView : Page -> (PageMsg -> msg) -> Html msg
pageView page toMsg =
  case page of
    HomePage homePageState ->
      homePageView homePageState (toMsg << HomePageMsg)
    NewPostPage newPostPageState ->
      newPostPageView newPostPageState (toMsg << NewPostPageMsg)
    ShowPostPage showPostPageState ->
      showPostPageView showPostPageState (toMsg << ShowPostPageMsg)
    LoginPage loginPageState ->
      loginPageView loginPageState (toMsg << LoginPageMsg)
    RegisterPage registerPageState ->
      registerPageView registerPageState (toMsg << RegisterPageMsg)
    AboutPage ->
      div [] [ text "about" ]
    NotFoundPage ->
      div [] [ text "not found" ]

--

type alias User =
  { id : Int
  , username : String
  , name : String
  , email : String 
  }

userDecoder : Json.Decoder User
userDecoder = 
  Json.map4 User
    (Json.field "id" Json.int)
    (Json.field "username" Json.string)
    (Json.field "name" Json.string)
    (Json.field "email" Json.string)

type alias Session =
  { user : User
  }

sessionDecoder : Json.Decoder Session
sessionDecoder = Json.map Session (Json.field "user" userDecoder)

--

type alias Flags =
  { api     : String
  , session : String }

type Msg
  = RouterMsg RouterMsg
  | PageMsg PageMsg

type alias State =
  { session : Maybe Session 
  , router : RouterState
  , page   : Page
  }

setSession : Maybe Session -> State -> Update State msg a
setSession session state = save { state | session = session }

inRouter : In State RouterState msg a
inRouter =
    inState { get = .router, set = \state router -> { state | router = router } }

inPage : In State Page msg a
inPage =
    inState { get = .page, set = \state page -> { state | page = page } }

initSession : Flags -> Maybe Session
initSession { session } =
  case Json.decodeString sessionDecoder session of
    Ok result ->
      Just result
    _ ->
      Nothing

init : Flags -> Url -> Navigation.Key -> Update State Msg a
init flags url key = 
  save State
    |> andMap (initSession flags |> save)
    |> andMap (routerInit key RouterMsg)
    |> andMap (save NotFoundPage)
    |> andThen (update (RouterMsg (UrlChange url)))

redirect : String -> State -> Update State msg a
redirect = inRouter << routerRedirect

ifAuthenticated : (State -> Update State msg a) -> State -> Update State msg a
ifAuthenticated gotoPage ({ session } as state) =
  state 
    |> if Nothing == session then redirect "/login" else gotoPage

unlessAuthenticated : (State -> Update State msg a) -> State -> Update State msg a
unlessAuthenticated gotoPage ({ session } as state) =
  state 
    |> if Nothing /= session then redirect "/" else gotoPage

loadPage : Update Page msg (State -> Update State msg a) -> State -> Update State msg a
loadPage = inPage << always

handleRouteChange : Maybe Route -> State -> Update State PageMsg a
handleRouteChange maybeRoute =
  case maybeRoute of
    -- No route
    Nothing ->
      loadPage (save NotFoundPage)

    -- Authenticated only
    Just NewPost ->
      newPostPageInit NewPostPageMsg
        |> Update.Deep.map NewPostPage
        |> loadPage 
        |> ifAuthenticated

    -- Redirect if already authenticated
    Just Login ->
      loginPageInit LoginPageMsg
        |> Update.Deep.map LoginPage
        |> loadPage 
        |> unlessAuthenticated

    Just Register ->
      registerPageInit RegisterPageMsg
        |> Update.Deep.map RegisterPage
        |> loadPage 
        |> unlessAuthenticated

    -- Other
    Just (ShowPost id) ->
      showPostPageInit ShowPostPageMsg
        |> Update.Deep.map ShowPostPage 
        |> loadPage 

    Just Home ->
      homePageInit HomePageMsg
        |> andThen (homePageUpdate FetchPosts HomePageMsg)
        |> Update.Deep.map HomePage
        |> loadPage 

    Just Logout ->
      setSession Nothing
        >> andThen (updateSessionStorage Nothing)
        >> andThen (redirect "/")

    Just About ->
      loadPage (save AboutPage)

updateSessionStorage : Maybe Session -> State -> Update State msg a
updateSessionStorage maybeSession =
  case maybeSession of
    Nothing ->
      addCmd (Ports.clearSession ())
    Just session ->
      addCmd (Ports.setSession session)

handleAuthResponse : Maybe Session -> State -> Update State Msg a
handleAuthResponse maybeSession = 
  setSession maybeSession
    >> andThen (updateSessionStorage maybeSession)
    >> andThenIf (Maybe.isJust maybeSession |> always) (redirect "/")

update : Msg -> State -> Update State Msg a
update msg =
  case msg of
    RouterMsg routerMsg ->
      inRouter (routerUpdate { onRouteChange = \route -> mapCmd PageMsg << handleRouteChange route } routerMsg)
    PageMsg pageMsg ->
      inPage (pageUpdate { onAuthResponse = handleAuthResponse, onPostAdded = always (redirect "/") } pageMsg PageMsg)

subscriptions : State -> Sub Msg
subscriptions { router, page } = 
  Sub.batch 
    [ routerSubscriptions router RouterMsg 
    , pageSubscriptions page PageMsg ]

view : State -> Document Msg
view ({ page } as state) = 
  { title = ""
  , body =
    [ text "hello"
    , pageView page PageMsg
    , div [] 
      [ a [ href "/" ] [ text "Home" ]
      , a [ href "/login" ] [ text "Login" ] 
      , a [ href "/logout" ] [ text "Logout" ]
      , a [ href "/register" ] [ text "Register" ]
      , a [ href "/about" ] [ text "About" ]
      , a [ href "/posts/new" ] [ text "New post" ]
      , Html.text (Debug.toString state)
      ]
    ] 
  }

main : Program Flags State Msg
main =
  Deep.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = RouterMsg << UrlChange
    , onUrlRequest  = RouterMsg << UrlRequest
    }

-- import Bootstrap.Button as Button
-- import Bootstrap.Form.Input as Input
-- import Bootstrap.Grid as Grid
-- import Bootstrap.Navbar as Navbar
-- import Bootstrap.Utilities.Spacing as Spacing
-- import Browser
-- import Browser exposing (Document, UrlRequest)
-- import Browser.Navigation as Navigation
-- import Form exposing (Form)
-- import Form.View
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import Http exposing (emptyBody)
-- import Json.Decode as Json exposing (Decoder, Value)
-- import Json.Encode as Encode exposing (object)
-- import UiFormView
-- import Update.Deep exposing (..)
-- import Update.Deep.Browser
-- import Url exposing (Url)
-- import Url.Parser as Parser exposing (Parser, parse, oneOf, (</>))
-- 
-- --
-- 
-- type ApiMsg a
--   = Request String (Maybe Http.Body)
--   | Response (Result Http.Error a)
--   | Reset
-- 
-- type ApiResource a
--   = NotRequested
--   | Requested
--   | Error Http.Error
--   | Available a
-- 
-- type alias Request a = String -> Maybe Http.Body -> Cmd (ApiMsg a)
-- 
-- type alias ApiModel a =
--   { resource : ApiResource a
--   , request  : Request a }
-- 
-- setResource : ApiResource a -> ApiModel a -> Update (ApiModel a) (ApiMsg a) b
-- setResource resource state = save { state | resource = resource }
-- 
-- type HttpMethod
--   = HttpGet
--   | HttpPost
-- 
-- type alias RequestConfig a =
--   { endpoint : String
--   , method   : HttpMethod
--   , decoder  : Json.Decoder a }
-- 
-- apiInit : RequestConfig a -> Update (ApiModel a) (ApiMsg a) b
-- apiInit { endpoint, method, decoder } =
--   let expect = Http.expectJson Response decoder
--       request suffix body =
--         case method of
--           HttpGet ->
--             Http.get
--               { url    = endpoint ++ suffix
--               , expect = expect }
--           HttpPost ->
--             Http.post
--               { url    = endpoint ++ suffix
--               , expect = expect
--               , body   = Maybe.withDefault emptyBody body }
--    in save
--     { resource = NotRequested
--     , request  = request }
-- 
-- type alias ApiHandlers a b =
--   { onSuccess : a -> b
--   , onError   : Http.Error -> b }
-- 
-- apiDefaultHandlers : ApiHandlers a (b -> Update b c e)
-- apiDefaultHandlers = { onSuccess = always save, onError = always save }
-- 
-- apiUpdate : ApiHandlers a b -> ApiMsg a -> ApiModel a -> Update (ApiModel a) (ApiMsg a) b
-- apiUpdate { onSuccess, onError } msg model =
--   case msg of
--     Request url maybeBody ->
--       model
--         |> setResource Requested
--         |> andAddCmd (model.request url maybeBody)
--     Response (Ok resource) ->
--       model
--         |> setResource (Available resource)
--         |> andInvokeHandler (onSuccess resource)
--     Response (Err error) ->
--       model
--         |> setResource (Error error)
--         |> andInvokeHandler (onError error)
--     Reset ->
--       model
--         |> setResource NotRequested
-- 
-- apiJsonRequest : String -> Value -> ApiMsg a
-- apiJsonRequest url = Request url << Just << Http.jsonBody
-- 
-- --
-- 
-- type FormMsg a
--   = UpdateViewModel (Form.View.Model a)
--   | ResetForm
--   | SubmitForm a
-- 
-- type alias FormModel a =
--   { viewModel : Form.View.Model a
--   , form      : Form a (FormMsg a)
--   , initial   : a }
-- 
-- type alias Fields a = Form a (FormMsg a)
-- 
-- formInit : Form a (FormMsg a) -> a -> Update (FormModel a) (FormMsg a) b
-- formInit form values =
--   save
--     { viewModel = Form.View.idle values
--     , form      = form
--     , initial   = values }
-- 
-- formUpdate : { onSubmit : a -> b } -> FormMsg a -> FormModel a -> Update (FormModel a) (FormMsg a) b
-- formUpdate { onSubmit } msg model =
--   case msg of
--     UpdateViewModel formViewModel ->
--       save { model | viewModel = formViewModel }
--     ResetForm ->
--       save { model | viewModel = Form.View.idle model.initial }
--     SubmitForm values ->
--       let { viewModel } = model
--        in save { model | viewModel = { viewModel | state = Form.View.Loading } }
--         |> andInvokeHandler (onSubmit values)
-- 
-- formView : FormModel a -> Html (FormMsg a)
-- formView { form, viewModel } =
--   UiFormView.view
--     { onChange   = UpdateViewModel
--     , action     = "Submit"
--     , loading    = "Submit"
--     , validation = Form.View.ValidateOnSubmit
--     } form viewModel
-- 
-- --
-- --
-- --
-- 
-- type Route
--  = Home
--  | About
--  | NewPost
--  | Post Int
--  | CommentPost Int
--  | Login
--  | Register
-- 
-- parser : Parser (Route -> a) a
-- parser =
--   oneOf
--     [ Parser.map Home        (Parser.top)
--     , Parser.map About       (Parser.s "about")
--     , Parser.map Login       (Parser.s "login")
--     , Parser.map Register    (Parser.s "register")
--     , Parser.map NewPost     (Parser.s "posts" </> Parser.s "new")
--     , Parser.map Post        (Parser.s "posts" </> Parser.int)
--     , Parser.map CommentPost (Parser.s "posts" </> Parser.int </> Parser.s "comment") ]
-- 
-- fromUrl : Url -> Maybe Route
-- fromUrl = parse parser
-- 
-- --
-- 
-- type alias DataUser =
--   { id    : Int
--   , email : String
--   , login : String
--   , name  : String }
-- 
-- dataUserDecoder : Json.Decoder DataUser
-- dataUserDecoder =
--   Json.map4 DataUser
--     (Json.field "id"    Json.int)
--     (Json.field "email" Json.string)
--     (Json.field "login" Json.string)
--     (Json.field "name"  Json.string)
-- 
-- --
-- 
-- type alias DataPost =
--   { id    : Int
--   , title : String
--   , body  : String }
-- 
-- dataPostDecoder : Json.Decoder DataPost
-- dataPostDecoder =
--   Json.map3 DataPost
--     (Json.field "id"    Json.int)
--     (Json.field "title" Json.string)
--     (Json.field "body"  Json.string)
-- 
-- --
-- 
-- type alias DataComment =
--   { id    : Int
--   , email : String
--   , body  : String }
-- 
-- dataCommentDecoder : Json.Decoder DataComment
-- dataCommentDecoder =
--   Json.map3 DataComment
--     (Json.field "id"    Json.int)
--     (Json.field "email" Json.string)
--     (Json.field "body"  Json.string)
-- 
-- --
-- 
-- type alias AuthRegisterForm =
--   { login    : String
--   , password : String }
-- 
-- authRegisterFormFields : Fields AuthRegisterForm
-- authRegisterFormFields =
-- 
--   let loginField =
--         Form.textField
--           { parser = Ok
--           , value  = .login
--           , update = \value values -> { values | login = value }
--           , attributes =
--             { label       = "Email"
--             , placeholder = "Email" } }
-- 
--       passwordField =
--         Form.passwordField
--           { parser = Ok
--           , value  = .password
--           , update = \value values -> { values | password = value }
--           , attributes =
--             { label       = "Password"
--             , placeholder = "Your password" } }
-- 
--    in Form.succeed AuthRegisterForm
--     |> Form.append loginField
--     |> Form.append passwordField
--     |> Form.map SubmitForm
-- 
-- authRegisterFormToJson : AuthRegisterForm -> Value
-- authRegisterFormToJson { login, password } =
--   object [ ( "login"    , Encode.string login )
--          , ( "password" , Encode.string password ) ]
-- 
-- --
-- 
-- type alias AuthLoginForm =
--   { login    : String
--   , password : String }
-- 
-- authLoginFormFields : Fields AuthLoginForm
-- authLoginFormFields =
-- 
--   let loginField =
--         Form.textField
--           { parser = Ok
--           , value  = .login
--           , update = \value values -> { values | login = value }
--           , attributes =
--             { label       = "Login"
--             , placeholder = "Login" } }
-- 
--       passwordField =
--         Form.passwordField
--           { parser = Ok
--           , value  = .password
--           , update = \value values -> { values | password = value }
--           , attributes =
--             { label       = "Password"
--             , placeholder = "Your password" } }
-- 
--    in Form.succeed AuthLoginForm
--     |> Form.append loginField
--     |> Form.append passwordField
--     |> Form.map SubmitForm
-- 
-- authLoginFormToJson : AuthLoginForm -> Value
-- authLoginFormToJson { login, password } =
--   object [ ( "login"    , Encode.string login )
--          , ( "password" , Encode.string password ) ]
-- 
-- --
-- 
-- type alias CommentsCreateForm =
--   { email   : String
--   , comment : String }
-- 
-- commentsCreateFormFields : Fields CommentsCreateForm
-- commentsCreateFormFields =
-- 
--   let emailField =
--         Form.textField
--           { parser = Ok
--           , value  = .email
--           , update = \value values -> { values | email = value }
--           , attributes =
--             { label       = "Email"
--             , placeholder = "Email" } }
-- 
--       commentField =
--         Form.textareaField
--           { parser = Ok
--           , value  = .comment
--           , update = \value values -> { values | comment = value }
--           , attributes =
--             { label       = "Comment"
--             , placeholder = "Comment" } }
-- 
--    in Form.succeed CommentsCreateForm
--     |> Form.append emailField
--     |> Form.append commentField
--     |> Form.map SubmitForm
-- 
-- commentsCreateFormToJson : CommentsCreateForm -> Value
-- commentsCreateFormToJson { email, comment } =
--   object [ ( "email"   , Encode.string email )
--          , ( "comment" , Encode.string comment ) ]
-- 
-- --
-- 
-- type alias PostsCreateForm =
--   { title : String
--   , body  : String }
-- 
-- postsCreateFormFields : Fields PostsCreateForm
-- postsCreateFormFields =
-- 
--   let titleField =
--         Form.textField
--           { parser = Ok
--           , value  = .title
--           , update = \value values -> { values | title = value }
--           , attributes =
--             { label       = "Title"
--             , placeholder = "Title" } }
-- 
--       bodyField =
--         Form.textareaField
--           { parser = Ok
--           , value  = .body
--           , update = \value values -> { values | body = value }
--           , attributes =
--             { label       = "Body"
--             , placeholder = "Body" } }
-- 
--    in Form.succeed PostsCreateForm
--     |> Form.append titleField
--     |> Form.append bodyField
--     |> Form.map SubmitForm
-- 
-- postsCreateFormToJson : PostsCreateForm -> Value
-- postsCreateFormToJson { title, body } =
--   object [ ( "title" , Encode.string title )
--          , ( "body"  , Encode.string body ) ]
-- 
-- --
-- 
-- type PostsCommentMsg
--   = PostsCommentApiPostMsg (ApiMsg DataPost)
--   | PostsCommentApiCommentMsg (ApiMsg DataComment)
--   | PostsCommentFormMsg (FormMsg CommentsCreateForm)
-- 
-- type alias PostsCommentModel =
--   { post    : ApiModel DataPost
--   , comment : ApiModel DataComment
--   , form    : FormModel CommentsCreateForm }
-- 
-- postsCommentInsertAsPostIn : PostsCommentModel -> ApiModel DataPost -> Update PostsCommentModel PostsCommentMsg a
-- postsCommentInsertAsPostIn model post = save { model | post = post }
-- 
-- postsCommentInsertAsCommentIn : PostsCommentModel -> ApiModel DataComment -> Update PostsCommentModel PostsCommentMsg a
-- postsCommentInsertAsCommentIn model comment = save { model | comment = comment }
-- 
-- postsCommentInsertAsFormIn : PostsCommentModel -> FormModel CommentsCreateForm -> Update PostsCommentModel PostsCommentMsg a
-- postsCommentInsertAsFormIn model form = save { model | form = form }
-- 
-- postsCommentInit : Int -> Update PostsCommentModel PostsCommentMsg a
-- postsCommentInit postId =
--   let post    = apiInit { endpoint = "/posts/" ++ String.fromInt postId
--                         , method   = HttpGet
--                         , decoder  = Json.field "post" dataPostDecoder }
--       comment = apiInit { endpoint = "/posts/" ++ String.fromInt postId ++ "/comments"
--                         , method   = HttpPost
--                         , decoder  = Json.field "post" dataCommentDecoder }
--       form = formInit commentsCreateFormFields { email = "", comment = "" }
--    in map3 PostsCommentModel
--         (post    |> mapCmd PostsCommentApiPostMsg)
--         (comment |> mapCmd PostsCommentApiCommentMsg)
--         (form    |> mapCmd PostsCommentFormMsg)
-- 
-- postsCommentUpdate : PostsCommentMsg -> PostsCommentModel -> Update PostsCommentModel PostsCommentMsg a
-- postsCommentUpdate msg model =
--   case msg of
--     PostsCommentApiPostMsg apiMsg ->
--       model.post
--         |> apiUpdate apiDefaultHandlers apiMsg
--         |> mapCmd PostsCommentApiPostMsg
--         |> andFinally (postsCommentInsertAsPostIn model)
--     PostsCommentApiCommentMsg apiMsg ->
--       model.comment
--         |> apiUpdate apiDefaultHandlers apiMsg
--         |> mapCmd PostsCommentApiCommentMsg
--         |> andFinally (postsCommentInsertAsCommentIn model)
--     PostsCommentFormMsg formMsg ->
--       model.form
--         |> formUpdate { onSubmit = always save } formMsg
--         |> mapCmd PostsCommentFormMsg
--         |> andFinally (postsCommentInsertAsFormIn model)
-- 
-- postsCommentSubscriptions : PostsCommentModel -> Sub PostsCommentMsg
-- postsCommentSubscriptions model = Sub.none
-- 
-- postsCommentView : PostsCommentModel -> Html PostsCommentMsg
-- postsCommentView { post, form } =
--   case post.resource of
--     NotRequested ->
--       div [] [ text "Not requested" ]
--     Requested ->
--       div [] [ text "Requested..." ]
--     Error error ->
--       div [] [ text "error" ]
--     Available item ->
--       div [] [ h1 []
--         [ text item.title
--         , Html.map PostsCommentFormMsg (formView form) ] ]
-- 
-- --
-- 
-- type PostsShowMsg
--   = PostsShowApiMsg (ApiMsg DataPost)
-- 
-- type alias PostsShowModel =
--   { post : ApiModel DataPost }
-- 
-- postsShowInsertAsPostIn : PostsShowModel -> ApiModel DataPost -> Update PostsShowModel PostsShowMsg a
-- postsShowInsertAsPostIn model post = save { model | post = post }
-- 
-- postsShowInit : Int -> Update PostsShowModel PostsShowMsg a
-- postsShowInit postId =
--   let post = apiInit { endpoint = "/posts/" ++ String.fromInt postId
--                      , method   = HttpGet
--                      , decoder  = Json.field "post" dataPostDecoder }
--    in Update.Deep.map PostsShowModel
--         (post |> mapCmd PostsShowApiMsg)
-- 
-- postsShowUpdate : PostsShowMsg -> PostsShowModel -> Update PostsShowModel PostsShowMsg a
-- postsShowUpdate msg model =
--   case msg of
--     PostsShowApiMsg apiMsg ->
--       model.post
--         |> apiUpdate apiDefaultHandlers apiMsg
--         |> mapCmd PostsShowApiMsg
--         |> andFinally (postsShowInsertAsPostIn model)
-- 
-- postsShowSubscriptions : PostsShowModel -> Sub PostsShowMsg
-- postsShowSubscriptions model = Sub.none
-- 
-- postsShowView : PostsShowModel -> Html PostsShowMsg
-- postsShowView { post } =
--   case post.resource of
--     NotRequested ->
--       div [] [ text "Not requested" ]
--     Requested ->
--       div [] [ text "Requested..." ]
--     Error error ->
--       div [] [ text "error" ]
--     Available item ->
--       div [] [ h1 [] [ text item.title ] ]
-- 
-- --
-- 
-- type HomePageMsg
--   = HomePageApiMsg (ApiMsg (List DataPost))
--   | FetchPosts
-- 
-- type alias HomePageModel =
--   { posts : ApiModel (List DataPost) }
-- 
-- insertAsPostsIn : HomePageModel -> ApiModel (List DataPost) -> Update HomePageModel HomePageMsg a
-- insertAsPostsIn model posts = save { model | posts = posts }
-- 
-- homePageInit : Maybe (List DataPost) -> Update HomePageModel HomePageMsg a
-- homePageInit maybePosts =
--   let resource =
--         case maybePosts of
--           Nothing -> NotRequested
--           Just posts_ -> Available posts_
--       posts = apiInit { endpoint = "/posts"
--                       , method   = HttpGet
--                       , decoder  = Json.field "posts" (Json.list dataPostDecoder) }
--                 |> andThen (setResource resource)
--    in Update.Deep.map HomePageModel
--         (posts |> mapCmd HomePageApiMsg)
-- 
-- homePageUpdate : { onPostsLoaded : List DataPost -> a } -> HomePageMsg -> HomePageModel -> Update HomePageModel HomePageMsg a
-- homePageUpdate { onPostsLoaded } msg model =
--   case msg of
--     HomePageApiMsg apiMsg ->
--       model.posts
--         |> apiUpdate { apiDefaultHandlers | onSuccess = \posts -> invokeHandler (onPostsLoaded posts) } apiMsg
--         |> mapCmd HomePageApiMsg
--         |> andFinally (insertAsPostsIn model)
--     FetchPosts ->
--       case model.posts.resource of
--         Requested ->
--           save model
--         Available _ ->
--           save model
--         _ ->
--           model
--             |> homePageUpdate { onPostsLoaded = onPostsLoaded } (HomePageApiMsg (Request "" Nothing))
-- 
-- homePageSubscriptions : HomePageModel -> Sub HomePageMsg
-- homePageSubscriptions model = Sub.none
-- 
-- listItem : DataPost -> Html HomePageMsg
-- listItem post =
--   let url = "/posts/" ++ String.fromInt post.id
--    in div []
--         [ h1 [] [ text post.title ]
--         , p [] [ text post.body ]
--         , p [] [ a [ href url ] [ text "Show" ]
--                , text " | "
--                , a [ href (url ++ "/comment") ] [ text "Comment" ] ] ]
-- 
-- homePageView : HomePageModel -> Html HomePageMsg
-- homePageView { posts } =
--   case posts.resource of
--     NotRequested ->
--       div [] [ text "Not requested"
--              , button [ onClick FetchPosts ] [ text "Fetch" ] ]
--     Requested ->
--       div [] [ text "Requested..." ]
--     Error error ->
--       div [] [ text "error" ]
--     Available list ->
--       div [] (List.map listItem list)
-- 
-- --
-- 
-- type PostsCreateMsg
--   = PostsCreateApiMsg (ApiMsg DataPost)
--   | PostsCreateFormMsg (FormMsg PostsCreateForm)
-- 
-- type alias PostsCreateModel =
--   { post : ApiModel DataPost
--   , form : FormModel PostsCreateForm }
-- 
-- insertAsPostIn : PostsCreateModel -> ApiModel DataPost -> Update PostsCreateModel PostsCreateMsg a
-- insertAsPostIn model post = save { model | post = post }
-- 
-- insertAsFormIn : PostsCreateModel -> FormModel PostsCreateForm -> Update PostsCreateModel PostsCreateMsg a
-- insertAsFormIn model form = save { model | form = form }
-- 
-- postsCreateInit : Update PostsCreateModel PostsCreateMsg a
-- postsCreateInit =
--   let api = apiInit { endpoint = "/posts"
--                     , method   = HttpPost
--                     , decoder  = Json.field "post" dataPostDecoder }
--       form = formInit postsCreateFormFields { title = "", body = "" }
--    in map2 PostsCreateModel
--         (api  |> mapCmd PostsCreateApiMsg)
--         (form |> mapCmd PostsCreateFormMsg)
-- 
-- postsCreateHandleSubmit : { onPostAdded : DataPost -> a } -> PostsCreateForm -> PostsCreateModel -> Update PostsCreateModel PostsCreateMsg a
-- postsCreateHandleSubmit events form model =
--   let json = postsCreateFormToJson form
--    in postsCreateUpdate events (PostsCreateApiMsg (apiJsonRequest "" json)) model
-- 
-- postsCreateUpdate : { onPostAdded : DataPost -> a } -> PostsCreateMsg -> PostsCreateModel -> Update PostsCreateModel PostsCreateMsg a
-- postsCreateUpdate { onPostAdded } msg model =
--   case msg of
--     PostsCreateApiMsg apiMsg ->
--       model.post
--         |> apiUpdate { apiDefaultHandlers | onSuccess = invokeHandler << onPostAdded } apiMsg
--         |> mapCmd PostsCreateApiMsg
--         |> andFinally (insertAsPostIn model)
--     PostsCreateFormMsg formMsg ->
--       model.form
--         |> formUpdate { onSubmit = postsCreateHandleSubmit { onPostAdded = onPostAdded } } formMsg
--         |> mapCmd PostsCreateFormMsg
--         |> andFinally (insertAsFormIn model)
-- 
-- postsCreateSubscriptions : PostsCreateModel -> Sub PostsCreateMsg
-- postsCreateSubscriptions model = Sub.none
-- 
-- postsCreateView : PostsCreateModel -> Html PostsCreateMsg
-- postsCreateView { form } =
--   Html.map PostsCreateFormMsg (formView form)
-- 
-- --
-- 
-- type AuthLoginMsg
--   = AuthLoginApiMsg (ApiMsg DataUser)
--   | AuthLoginFormMsg (FormMsg AuthLoginForm)
-- 
-- type alias AuthLoginModel =
--   { user : ApiModel DataUser
--   , form : FormModel AuthLoginForm }
-- 
-- authLoginInsertAsUserIn : AuthLoginModel -> ApiModel DataUser -> Update AuthLoginModel AuthLoginMsg a
-- authLoginInsertAsUserIn model user = save { model | user = user }
-- 
-- authLoginInsertAsFormIn : AuthLoginModel -> FormModel AuthLoginForm -> Update AuthLoginModel AuthLoginMsg a
-- authLoginInsertAsFormIn model form = save { model | form = form }
-- 
-- authLoginInit : Update AuthLoginModel AuthLoginMsg a
-- authLoginInit =
--   let api = apiInit { endpoint = "/auth/login"
--                     , method   = HttpPost
--                     , decoder  = Json.field "user" dataUserDecoder }
--       form = formInit authLoginFormFields { login = "", password = "" }
--    in map2 AuthLoginModel
--         (api  |> mapCmd AuthLoginApiMsg)
--         (form |> mapCmd AuthLoginFormMsg)
-- 
-- authLoginHandleSubmit : { onResponse : Maybe DataUser -> a } -> AuthLoginForm -> AuthLoginModel -> Update AuthLoginModel AuthLoginMsg a
-- authLoginHandleSubmit events form model =
--   let json = authLoginFormToJson form
--    in authLoginUpdate events (AuthLoginApiMsg (apiJsonRequest "" json)) model
-- 
-- authLoginUpdate : { onResponse : Maybe DataUser -> a } -> AuthLoginMsg -> AuthLoginModel -> Update AuthLoginModel AuthLoginMsg a
-- authLoginUpdate { onResponse } msg model =
--   case msg of
--     AuthLoginApiMsg apiMsg ->
--       model.user
--         |> apiUpdate { onSuccess = invokeHandler << onResponse << Just
--                      , onError   = always (invokeHandler (onResponse Nothing)) } apiMsg
--         |> mapCmd AuthLoginApiMsg
--         |> andFinally (authLoginInsertAsUserIn model)
--     AuthLoginFormMsg formMsg ->
--       model.form
--         |> formUpdate { onSubmit = authLoginHandleSubmit { onResponse = onResponse } } formMsg
--         |> mapCmd AuthLoginFormMsg
--         |> andFinally (authLoginInsertAsFormIn model)
-- 
-- authLoginSubscriptions : AuthLoginModel -> Sub AuthLoginMsg
-- authLoginSubscriptions model = Sub.none
-- 
-- authLoginView : AuthLoginModel -> Html AuthLoginMsg
-- authLoginView { form } =
--   div []
--     [ h1 [] [ text "Log in" ]
--     , Html.map AuthLoginFormMsg (formView form) ]
-- 
-- --
-- 
-- type AuthRegisterMsg
--   = AuthRegisterApiMsg (ApiMsg { status : String })
--   | AuthRegisterFormMsg (FormMsg AuthRegisterForm)
-- 
-- type alias AuthRegisterModel =
--   { response : ApiModel { status : String }
--   , form     : FormModel AuthRegisterForm }
-- 
-- authRegisterInsertAsResponseIn : AuthRegisterModel -> ApiModel { status : String } -> Update AuthRegisterModel AuthRegisterMsg a
-- authRegisterInsertAsResponseIn model response = save { model | response = response }
-- 
-- authRegisterInsertAsFormIn : AuthRegisterModel -> FormModel AuthRegisterForm -> Update AuthRegisterModel AuthRegisterMsg a
-- authRegisterInsertAsFormIn model form = save { model | form = form }
-- 
-- authRegisterInit : Update AuthRegisterModel AuthRegisterMsg a
-- authRegisterInit =
--   let decoder = Json.field "status" Json.string |> Json.map (\status -> { status = status })
--       api = apiInit { endpoint = "/auth/register"
--                     , method   = HttpPost
--                     , decoder  = decoder }
--       form = formInit authRegisterFormFields { login = "", password = "" }
--    in map2 AuthRegisterModel
--         (api  |> mapCmd AuthRegisterApiMsg)
--         (form |> mapCmd AuthRegisterFormMsg)
-- 
-- authRegisterUpdate : AuthRegisterMsg -> AuthRegisterModel -> Update AuthRegisterModel AuthRegisterMsg a
-- authRegisterUpdate msg model =
--   case msg of
--     AuthRegisterApiMsg apiMsg ->
--       model.response
--         |> apiUpdate apiDefaultHandlers apiMsg
--         |> mapCmd AuthRegisterApiMsg
--         |> andFinally (authRegisterInsertAsResponseIn model)
--     AuthRegisterFormMsg formMsg ->
--       model.form
--         |> formUpdate { onSubmit = always save } formMsg
--         |> mapCmd AuthRegisterFormMsg
--         |> andFinally (authRegisterInsertAsFormIn model)
-- 
-- authRegisterSubscriptions : AuthRegisterModel -> Sub AuthRegisterMsg
-- authRegisterSubscriptions model = Sub.none
-- 
-- authRegisterView : AuthRegisterModel -> Html AuthRegisterMsg
-- authRegisterView { form } =
--   Html.map AuthRegisterFormMsg (formView form)
-- 
-- --
-- 
-- type RouterMsg
--   = UrlChange Url
--   | UrlRequest UrlRequest
--   | Redirect String
--   | Restricted String
--   | ReturnToRestricted
-- 
-- type alias RouterModel =
--   { route      : Maybe Route
--   , key        : Navigation.Key
--   , restricted : Maybe String
--   , returnUrl  : Maybe String }
-- 
-- setRoute : Maybe Route -> RouterModel -> Update RouterModel RouterMsg a
-- setRoute route model = save { model | route = route }
-- 
-- setRestricted : Maybe String -> RouterModel -> Update RouterModel RouterMsg a
-- setRestricted restricted model = save { model | restricted = restricted }
-- 
-- setReturnUrl : Maybe String -> RouterModel -> Update RouterModel RouterMsg a
-- setReturnUrl url model = save { model | returnUrl = url }
-- 
-- routerInit : Flags -> Url -> Navigation.Key -> Update RouterModel RouterMsg a
-- routerInit flags url key = save { route = Nothing, key = key, restricted = Nothing, returnUrl = Nothing }
-- 
-- routerUpdate : { onRouteChange : Maybe Route -> a } -> RouterMsg -> RouterModel -> Update RouterModel RouterMsg a
-- routerUpdate { onRouteChange } msg model =
--   let redirect = routerUpdate { onRouteChange = onRouteChange } << Redirect
--    in case msg of
--     UrlChange url ->
--       let route = fromUrl url
--        in model
--         |> setRoute route
--         |> andThen (setReturnUrl model.restricted)
--         |> andThen (setRestricted Nothing)
--         |> andInvokeHandler (onRouteChange route)
--     UrlRequest (Browser.Internal url) ->
--       model
--         |> runCmd (Navigation.pushUrl model.key (Url.toString url))
--     UrlRequest (Browser.External href) ->
--       model
--         |> runCmd (Navigation.load href)
--     Redirect href ->
--       model
--         |> runCmd (Navigation.replaceUrl model.key href)
--     Restricted route ->
--       model
--         |> setRestricted (Just route)
--         |> andThen (redirect "/login")
--     ReturnToRestricted ->
--       model
--         |> case model.returnUrl of
--              Nothing ->
--                redirect "/"
--              Just href ->
--                redirect href
-- 
-- routerSubscriptions : RouterModel -> Sub RouterMsg
-- routerSubscriptions model = Sub.none
-- 
-- --
-- 
-- type UiMsg
--   = NavbarMsg Navbar.State
--   | UiLogOut
-- 
-- type alias UiModel =
--   { navbar : Navbar.State }
-- 
-- uiInit : Flags -> Url -> Navigation.Key -> Update UiModel UiMsg a
-- uiInit flags url key =
--   let ( navbar, cmd ) = Navbar.initialState NavbarMsg
--    in Update.Deep.map UiModel
--         (runCmd cmd navbar)
-- 
-- uiUpdate : { onLogOut : a } -> UiMsg -> UiModel -> Update UiModel UiMsg a
-- uiUpdate { onLogOut } msg model =
--   case msg of
--     NavbarMsg navbarState ->
--       save { model | navbar = navbarState }
--     UiLogOut ->
--       model
--         |> invokeHandler onLogOut
-- 
-- uiSubscriptions : UiModel -> Sub UiMsg
-- uiSubscriptions model = Navbar.subscriptions model.navbar NavbarMsg
-- 
-- uiNavbarView : UiModel -> Maybe DataUser -> Html UiMsg
-- uiNavbarView model maybeUser =
--   let button =
--         case maybeUser of
--           Nothing ->
--             Button.linkButton
--               [ Button.success
--               , Button.attrs [ href "/login", Spacing.ml2Sm ] ]
--               [ text "Log in"]
--           Just _ ->
--             Button.linkButton
--               [ Button.primary
--               , Button.attrs [ href "", Spacing.ml2Sm, onClick UiLogOut ] ]
--               [ text "Log out"]
-- 
--    in Navbar.config NavbarMsg
--     |> Navbar.withAnimation
--     |> Navbar.brand [ href "/"] [ text "FooPress"]
--     |> Navbar.items
--         [ Navbar.itemLink [ href "/posts/new" ] [ text "New post"]
--         , Navbar.itemLink [ href "/about" ] [ text "About" ] ]
--     |> Navbar.customItems
--         [ Navbar.formItem []
--           [ button ]
--         ]
--     |> Navbar.view model.navbar
-- 
-- --
-- 
-- type CacheData
--   = CachePosts (List DataPost)
-- 
-- type CacheMsg
--   = InsertData CacheData
--   | PurgePosts
-- 
-- type alias CacheModel =
--   { posts : Maybe (List DataPost) }
-- 
-- cacheInit : Update CacheModel CacheMsg a
-- cacheInit = save { posts = Nothing }
-- 
-- cacheUpdate : CacheMsg -> CacheModel -> Update CacheModel CacheMsg a
-- cacheUpdate msg model =
--   case msg of
--     InsertData (CachePosts posts) ->
--       save { model | posts = Just posts }
--     PurgePosts ->
--       save { model | posts = Nothing }
-- 
-- --
-- 
-- type PageMsg
--   = PostsCreateMsg PostsCreateMsg
--   | HomePageMsg HomePageMsg
--   | PostsShowMsg PostsShowMsg
--   | PostsCommentMsg PostsCommentMsg
--   | AuthLoginMsg AuthLoginMsg
--   | AuthRegisterMsg AuthRegisterMsg
-- 
-- type Page
--   = HomePage HomePageModel
--   | AboutPage
--   | NewPostPage PostsCreateModel
--   | ShowPostPage PostsShowModel
--   | CommentPostPage PostsCommentModel
--   | LoginPage AuthLoginModel
--   | RegisterPage AuthRegisterModel
--   | NotFoundPage
-- 
-- handlePostAdded : { redirect : String -> a, updateCache : CacheMsg -> a } -> DataPost -> PostsCreateModel -> Update PostsCreateModel PageMsg a
-- handlePostAdded { redirect, updateCache } post model =
--   model
--     |> invokeHandler (updateCache PurgePosts)
--     |> andInvokeHandler (redirect "/")
-- 
-- pageUpdate : { redirect : String -> a, onUserAuth : Maybe DataUser -> a, updateCache : CacheMsg -> a } -> PageMsg -> Page -> Update Page PageMsg a
-- pageUpdate { redirect, onUserAuth, updateCache } msg page =
--   case ( msg, page ) of
--     ( PostsCreateMsg postsCreateMsg, NewPostPage postsCreateModel ) ->
--       postsCreateModel
--         |> postsCreateUpdate { onPostAdded = handlePostAdded { redirect = redirect, updateCache = updateCache } } postsCreateMsg
--         |> mapCmd PostsCreateMsg
--         |> foldEvents
--         |> Update.Deep.map NewPostPage
--     ( HomePageMsg homePageMsg, HomePage homePageModel ) ->
--       homePageModel
--         |> homePageUpdate { onPostsLoaded = invokeHandler << updateCache << InsertData << CachePosts } homePageMsg
--         |> mapCmd HomePageMsg
--         |> foldEvents
--         |> Update.Deep.map HomePage
--     ( PostsShowMsg postsShowMsg, ShowPostPage postsShowModel ) ->
--       postsShowModel
--         |> postsShowUpdate postsShowMsg
--         |> mapCmd PostsShowMsg
--         |> foldEvents
--         |> Update.Deep.map ShowPostPage
--     ( PostsCommentMsg postsCommentMsg, CommentPostPage postsCommentModel ) ->
--       postsCommentModel
--         |> postsCommentUpdate postsCommentMsg
--         |> mapCmd PostsCommentMsg
--         |> foldEvents
--         |> Update.Deep.map CommentPostPage
--     ( AuthLoginMsg authLoginMsg, LoginPage authLoginModel ) ->
--       authLoginModel
--         |> authLoginUpdate { onResponse = invokeHandler << onUserAuth } authLoginMsg
--         |> mapCmd AuthLoginMsg
--         |> foldEvents
--         |> Update.Deep.map LoginPage
--     ( AuthRegisterMsg authRegisterMsg, RegisterPage authRegisterModel ) ->
--       authRegisterModel
--         |> authRegisterUpdate authRegisterMsg
--         |> mapCmd AuthRegisterMsg
--         |> foldEvents
--         |> Update.Deep.map RegisterPage
--     _ ->
--       save page
-- 
-- pageSubscriptions : Page -> Int -> Sub Msg
-- pageSubscriptions page pageId =
--   case page of
--     HomePage homePageModel ->
--       Sub.map (PageMsg pageId << HomePageMsg) (homePageSubscriptions homePageModel)
--     NewPostPage postsCreateModel ->
--       Sub.map (PageMsg pageId << PostsCreateMsg) (postsCreateSubscriptions postsCreateModel)
--     ShowPostPage postsShowModel ->
--       Sub.map (PageMsg pageId << PostsShowMsg) (postsShowSubscriptions postsShowModel)
--     CommentPostPage postsCommentModel ->
--       Sub.map (PageMsg pageId << PostsCommentMsg) (postsCommentSubscriptions postsCommentModel)
--     LoginPage authLoginModel ->
--       Sub.map (PageMsg pageId << AuthLoginMsg) (authLoginSubscriptions authLoginModel)
--     RegisterPage authRegisterModel ->
--       Sub.map (PageMsg pageId << AuthRegisterMsg) (authRegisterSubscriptions authRegisterModel)
--     AboutPage ->
--       Sub.none
--     NotFoundPage ->
--       Sub.none
-- 
-- pageView : Page -> Int -> Html Msg
-- pageView page pageId =
--   case page of
--     NewPostPage postsCreateModel ->
--       Html.map (PageMsg pageId << PostsCreateMsg) (postsCreateView postsCreateModel)
--     ShowPostPage postsShowModel ->
--       Html.map (PageMsg pageId << PostsShowMsg) (postsShowView postsShowModel)
--     CommentPostPage postsCommentModel ->
--       Html.map (PageMsg pageId << PostsCommentMsg) (postsCommentView postsCommentModel)
--     HomePage homePageModel ->
--       Html.map (PageMsg pageId << HomePageMsg) (homePageView homePageModel)
--     LoginPage authLoginModel ->
--       Html.map (PageMsg pageId << AuthLoginMsg) (authLoginView authLoginModel)
--     RegisterPage authRegisterModel ->
--       Html.map (PageMsg pageId << AuthRegisterMsg) (authRegisterView authRegisterModel)
--     AboutPage ->
--       div [] [ text "about" ]
--     NotFoundPage ->
--       div [] [ text "not found" ]
-- 
-- --
-- 
-- type Msg
--   = RouterMsg RouterMsg
--   | UiMsg UiMsg
--   | CacheMsg CacheMsg
--   | PageMsg Int PageMsg
--   | LogOut
-- 
-- type alias Flags = ()
-- 
-- type alias Model =
--   { router : RouterModel
--   , ui     : UiModel
--   , cache  : CacheModel
--   , page   : Page
--   , pageId : Int
--   , user   : Maybe DataUser }
-- 
-- insertAsRouterIn : Model -> RouterModel -> Update Model Msg a
-- insertAsRouterIn model router = save { model | router = router }
-- 
-- insertAsUiIn : Model -> UiModel -> Update Model Msg a
-- insertAsUiIn model ui = save { model | ui = ui }
-- 
-- insertAsPageIn : Model -> Page -> Update Model Msg a
-- insertAsPageIn model page = save { model | page = page }
-- 
-- insertAsCacheIn : Model -> CacheModel -> Update Model Msg a
-- insertAsCacheIn model cache = save { model | cache = cache }
-- 
-- setUser : Maybe DataUser -> Model -> Update Model Msg a
-- setUser user model = save { model | user = user }
-- 
-- incrementPageId : Model -> Update Model Msg a
-- incrementPageId model = save { model | pageId = 1 + model.pageId }
-- 
-- init : Flags -> Url -> Navigation.Key -> Update Model Msg a
-- init flags url key =
--   let router = routerInit flags url key
--       ui     = uiInit flags url key
--       cache  = save { posts = Nothing }
--    in map6 Model
--         (router |> mapCmd RouterMsg)
--         (ui     |> mapCmd UiMsg)
--         (cache  |> mapCmd CacheMsg)
--         (save NotFoundPage)
--         (save 0)
--         (save Nothing)
--           |> andThen (update (RouterMsg (UrlChange url)))
-- 
-- loadPage : Maybe Route -> Model -> Update Model Msg a
-- loadPage route model =
--   case route of
--     Just Home ->
--       homePageInit model.cache.posts
--         |> mapCmd (PageMsg model.pageId << HomePageMsg)
--         |> andThen (\pageModel -> insertAsPageIn model (HomePage pageModel))
--         |> andThen (update (PageMsg model.pageId (HomePageMsg FetchPosts)))
--     Just NewPost ->
--       if Nothing == model.user
--           then model
--             |> updateRouterWith (Restricted "/posts/new")
--           else postsCreateInit
--             |> mapCmd (PageMsg model.pageId << PostsCreateMsg)
--             |> andThen (\pageModel -> insertAsPageIn model (NewPostPage pageModel))
--     Just (Post id) ->
--       postsShowInit id
--         |> mapCmd (PageMsg model.pageId << PostsShowMsg)
--         |> andThen (\pageModel -> insertAsPageIn model (ShowPostPage pageModel))
--     Just (CommentPost postId) ->
--       postsCommentInit postId
--         |> mapCmd (PageMsg model.pageId << PostsCommentMsg)
--         |> andThen (\pageModel -> insertAsPageIn model (CommentPostPage pageModel))
--     Just Login ->
--       authLoginInit
--         |> mapCmd (PageMsg model.pageId << AuthLoginMsg)
--         |> andThen (\pageModel -> insertAsPageIn model (LoginPage pageModel))
--     Just Register ->
--       authRegisterInit
--         |> mapCmd (PageMsg model.pageId << AuthRegisterMsg)
--         |> andThen (\pageModel -> insertAsPageIn model (RegisterPage pageModel))
--     _ ->
--       save { model | page = NotFoundPage }
-- 
-- handleRouteChange : Maybe Route -> Model -> Update Model Msg a
-- handleRouteChange route model =
--   model
--     |> incrementPageId
--     |> andThen (loadPage route)
-- 
-- handleLogOut : Model -> Update Model Msg a
-- handleLogOut model =
--   model
--     |> setUser Nothing
--     |> andAddCmd Navigation.reload
-- 
-- updateRouterWith : RouterMsg -> Model -> Update Model Msg a
-- updateRouterWith msg model =
--   model.router
--     |> routerUpdate { onRouteChange = handleRouteChange } msg
--     |> mapCmd RouterMsg
--     |> andFinally (insertAsRouterIn model)
-- 
-- updateUiWith : UiMsg -> Model -> Update Model Msg a
-- updateUiWith msg model =
--   model.ui
--     |> uiUpdate { onLogOut = handleLogOut } msg
--     |> mapCmd UiMsg
--     |> andFinally (insertAsUiIn model)
-- 
-- updateCacheWith : CacheMsg -> Model -> Update Model Msg a
-- updateCacheWith msg model =
--   model.cache
--     |> cacheUpdate msg
--     |> mapCmd CacheMsg
--     |> andFinally (insertAsCacheIn model)
-- 
-- handleUserAuthenticated : Maybe DataUser -> Model -> Update Model Msg a
-- handleUserAuthenticated user model =
--   ( model
--       |> case user of
--            Nothing ->
--              save
--            Just _ ->
--              updateRouterWith ReturnToRestricted
--   ) |> andThen (setUser user)
-- 
-- updatePageWith : PageMsg -> Model -> Update Model Msg a
-- updatePageWith msg model =
--   let handlers = { redirect    = update << RouterMsg << Redirect
--                  , updateCache = update << CacheMsg
--                  , onUserAuth  = handleUserAuthenticated }
--    in model.page
--     |> pageUpdate handlers msg
--     |> mapCmd (PageMsg model.pageId)
--     |> andFinally (insertAsPageIn model)
-- 
-- update : Msg -> Model -> Update Model Msg a
-- update msg model =
--   case msg of
--     RouterMsg routerMsg ->
--       model
--         |> updateRouterWith routerMsg
--     UiMsg uiMsg ->
--       model
--         |> updateUiWith uiMsg
--     CacheMsg cacheMsg ->
--       model
--         |> updateCacheWith cacheMsg
--     PageMsg pageId pageMsg ->
--       model
--         |> if pageId == model.pageId
--                then updatePageWith pageMsg
--                else save
--     LogOut ->
--       save { model | user = Nothing }
-- 
-- subscriptions : Model -> Sub Msg
-- subscriptions { page, pageId, router, ui } =
--   Sub.batch
--     ( pageSubscriptions page pageId :: [ Sub.map RouterMsg (routerSubscriptions router)
--                                        , Sub.map UiMsg (uiSubscriptions ui) ] )
-- 
-- view : Model -> Document Msg
-- view model =
--   { title = ""
--   , body =
--     [ Html.map UiMsg (uiNavbarView model.ui model.user)
--     , Grid.container []
--       [ Grid.row []
--         [ Grid.col []
--           [ pageView model.page model.pageId ]
--         ]
--       , text (Debug.toString model)
--       ]
--     ]
-- 
--     --[ div []
--     --  [ text "Hello"
--     --  , ul []
--     --    [ li [] [ a [ href "/" ] [ text "Home" ] ]
--     --    , li [] [ a [ href "/posts/new" ] [ text "New post" ] ]
--     --    , li [] [ a [ href "/login" ] [ text "Login" ] ]
--     --    , li [] [ a [ href "/register" ] [ text "Register" ] ]
--     --    , pageView model.page model.pageId ]
--     --  , text (Debug.toString model)
--     --  ]
--     --]
--   }
-- 
-- --
-- 
-- main : Program Flags Model Msg
-- main =
--   Update.Deep.Browser.application
--     { init          = init
--     , update        = update
--     , subscriptions = subscriptions
--     , view          = view
--     , onUrlChange   = RouterMsg << UrlChange
--     , onUrlRequest  = RouterMsg << UrlRequest }
