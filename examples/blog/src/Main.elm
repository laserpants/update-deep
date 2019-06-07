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
import Bulma.CDN as Bulma
import Bulma.Elements as Bulma
import Bulma.Components exposing (..)
import Bulma.Modifiers exposing (..)

--

type UiMsg
  = ToggleBurgerMenu

type alias UiState =
  { menuOpen : Bool }

toggleMenuOpen : UiState -> Update UiState msg a
toggleMenuOpen state = save { state | menuOpen = not state.menuOpen }

myNavbar : Page -> UiState -> (UiMsg -> msg) -> Html msg
myNavbar page { menuOpen } toMsg = 

  let 
      burger = 
        navbarBurger menuOpen [ onClick ToggleBurgerMenu ] 
          [ span [] [], span [] [], span [] [] ]

      currentPage = current page

   in
        navbar navbarModifiers []
          [ navbarBrand [] burger
            [ navbarItem False [] [ text "hello" ] ]
          , navbarMenu menuOpen []
            [ navbarStart [] 
              [ navbarItemLink currentPage.isHomePage [ href "/" ] [ text "Home" ]
              , navbarItemLink currentPage.isAboutPage [ href "/about" ] [ text "About" ]
              , navbarItemLink currentPage.isNewPostPage [ href "/posts/new" ] [ text "New post" ]
              ]
              , navbarEnd [] 
                [ navbarItem False [] 
                  [ div [ class "field is-grouped" ] 
                    [ p [ class "control" ] [ Bulma.easyButton Bulma.buttonModifiers [] ToggleBurgerMenu "Log in" ] 
                    , p [ class "control" ] [ Bulma.easyButton Bulma.buttonModifiers [] ToggleBurgerMenu "Register" ] 
                    ] 
                  ] 
                ]
            ]
          ]
        |> Html.map toMsg

uiInit : Update UiState msg a
uiInit = 
  save UiState
    |> andMap (save False)

uiUpdate : UiMsg -> (UiMsg -> msg) -> UiState -> Update UiState msg a
uiUpdate msg toMsg state =
  case msg of
    ToggleBurgerMenu ->
      state
        |> toggleMenuOpen

--

type alias CommentForm =
  { email : String
  , body : String 
  }

commentFormValidate : Validation Never CommentForm
commentFormValidate =
  succeed CommentForm
    |> Validate.andMap (field "email" validateEmail)
    |> Validate.andMap (field "body" validateStringNonEmpty)

commentFormToJson : Int -> CommentForm -> Json.Value
commentFormToJson postId { email, body } = 
  object 
    [ ( "postId" , Json.Encode.int postId )
    , ( "email" , Json.Encode.string email )
    , ( "body" , Json.Encode.string body )
    ]

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

type alias Comment =
  { id : Int
  , postId : Int
  , email : String
  , body : String
  }

commentDecoder : Json.Decoder Comment
commentDecoder =
  Json.map4 Comment
    (Json.field "id"    Json.int)
    (Json.field "postId" Json.int)
    (Json.field "email" Json.string)
    (Json.field "body"  Json.string)

--

type alias Post =
  { id    : Int
  , title : String
  , body  : String 
  , comments : List Comment
  }

postDecoder : Json.Decoder Post
postDecoder =
  Json.map4 Post
    (Json.field "id"    Json.int)
    (Json.field "title" Json.string)
    (Json.field "body"  Json.string)
    (Json.field "comments" (Json.list commentDecoder))

--

validateStringNonEmpty : Field -> Result (Error e) String
validateStringNonEmpty = 
  [ Validate.string, Validate.emptyString ]
    |> Validate.oneOf  
    |> Validate.andThen Validate.nonEmpty

validateEmail : Field -> Result (Error e) String
validateEmail = 
  validateStringNonEmpty 
    |> Validate.andThen (always Validate.email) 

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
          , p [] [ text post.body ] 
          , p [] [ text (let count = List.length post.comments in if count > 0 then (String.fromInt count ++ " comment(s)") else "No comments") ] 
          , a [ href ("/posts/" ++ String.fromInt post.id) ] [ text "Show post" ]
          ]

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
    [ h2 [] [ text "Posts" ]
    , homePagePostsList state.posts
    ]

--

type alias NewPostForm =
  { title : String
  , body : String 
  }

newPostFormValidate : Validation Never NewPostForm
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
  , formModel : FormModel Never NewPostForm }

newPostPageInApi : In NewPostPageState (ApiModel Post) msg a
newPostPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

newPostPageInForm : In NewPostPageState (FormModel Never NewPostForm) msg a
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

newPostPageHandleSubmit : (NewPostPageMsg -> msg) -> NewPostForm -> NewPostPageState -> Update NewPostPageState msg a
newPostPageHandleSubmit toMsg form = 
  let json = form |> newPostFormToJson |> Http.jsonBody 
   in newPostPageInApi (apiSendRequest "" (Just json) (toMsg << NewPostPageApiMsg))

newPostPageUpdate : { onPostAdded : Post -> a } -> NewPostPageMsg -> (NewPostPageMsg -> msg) -> NewPostPageState -> Update NewPostPageState msg a
newPostPageUpdate { onPostAdded } msg toMsg = 
  case msg of
    NewPostPageApiMsg apiMsg ->
      newPostPageInApi (apiUpdate { onSuccess = invokeHandler << onPostAdded, onError = always save } apiMsg (toMsg << NewPostPageApiMsg))
    NewPostFormMsg formMsg ->
      newPostPageInForm (formUpdate { onSubmit = newPostPageHandleSubmit toMsg } formMsg)

newPostPageSubscriptions : NewPostPageState -> (NewPostPageMsg -> msg) -> Sub msg
newPostPageSubscriptions state toMsg = Sub.none

newPostPageFormView : FormModel Never NewPostForm -> (Form.Msg -> msg) -> Html msg
newPostPageFormView { form, disabled } toMsg =

  let title = form |> Form.getFieldAsString "title"
      body  = form |> Form.getFieldAsString "body"

      alwaysEmpty = always ""

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack alwaysEmpty (errorToString alwaysEmpty)

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
  = ShowPostPageApiMsg (ApiMsg Post)
  | ShowPostPageCommentApiMsg (ApiMsg Comment)
  | FetchPost
  | ShowPostPageCommentFormMsg Form.Msg

type alias ShowPostPageState =
  { id : Int
  , post : ApiModel Post 
  , comment : ApiModel Comment
  , commentForm : FormModel Never CommentForm }

showPostPageInApi : In ShowPostPageState (ApiModel Post) msg a
showPostPageInApi =
    inState { get = .post, set = \state post -> { state | post = post } }

showPostPageInCommentApi : In ShowPostPageState (ApiModel Comment) msg a
showPostPageInCommentApi =
    inState { get = .comment, set = \state comment -> { state | comment = comment } }

showPostPageInCommentForm : In ShowPostPageState (FormModel Never CommentForm) msg a
showPostPageInCommentForm =
    inState { get = .commentForm, set = \state form -> { state | commentForm = form } }

showPostPageInit : Int -> (ShowPostPageMsg -> msg) -> Update ShowPostPageState msg a
showPostPageInit id toMsg = 

  let
      post = apiInit { endpoint = "/posts/" ++ String.fromInt id
                     , method   = HttpGet
                     , decoder  = Json.field "post" postDecoder }

      comment = apiInit { endpoint = "/posts/" ++ String.fromInt id ++ "/comments"
                        , method   = HttpPost
                        , decoder  = Json.field "comment" commentDecoder }

      form = formInit [] commentFormValidate
   in 
      save ShowPostPageState
        |> andMap (save id)
        |> andMap post
        |> andMap comment
        |> andMap form
        |> mapCmd toMsg

showPostPageHandleSubmit : (ShowPostPageMsg -> msg) -> CommentForm -> ShowPostPageState -> Update ShowPostPageState msg a
showPostPageHandleSubmit toMsg form state = 
  let 
      json = form |> commentFormToJson state.id |> Http.jsonBody 
   in 
      state 
        |> showPostPageInCommentApi (apiSendRequest "" (Just json) (toMsg << ShowPostPageCommentApiMsg))

showPostPageUpdate : ShowPostPageMsg -> (ShowPostPageMsg -> msg) -> ShowPostPageState -> Update ShowPostPageState msg a
showPostPageUpdate msg toMsg = 

  let 
      commentCreated comment = 
        showPostPageInCommentForm (formReset [])
          >> andThen (showPostPageUpdate FetchPost toMsg)
   in 
      case msg of
        ShowPostPageApiMsg apiMsg ->
          showPostPageInApi (apiUpdate { onSuccess = always save, onError = always save } apiMsg (toMsg << ShowPostPageApiMsg))
        FetchPost ->
          showPostPageInApi (apiSendRequest "" Nothing (toMsg << ShowPostPageApiMsg))
        ShowPostPageCommentFormMsg formMsg ->
          showPostPageInCommentForm (formUpdate { onSubmit = showPostPageHandleSubmit toMsg } formMsg)
        ShowPostPageCommentApiMsg apiMsg ->
          showPostPageInCommentApi (apiUpdate { onSuccess = commentCreated, onError = always save } apiMsg (toMsg << ShowPostPageCommentApiMsg))

showPostPageSubscriptions : ShowPostPageState -> (ShowPostPageMsg -> msg) -> Sub msg
showPostPageSubscriptions state toMsg = Sub.none

newPostPageCommentFormView : FormModel Never CommentForm -> (Form.Msg -> msg) -> Html msg
newPostPageCommentFormView { form, disabled } toMsg =

  let email = form |> Form.getFieldAsString "email"
      body  = form |> Form.getFieldAsString "body"

      alwaysEmpty = always ""

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack alwaysEmpty (errorToString alwaysEmpty)

   in

      [ fieldset [ Html.Attributes.disabled disabled ]
        [ div [] 
          [ input 
            [ onFocus (Form.Focus email.path)
            , onBlur (Form.Blur email.path)
            , onInput (String >> Form.Input email.path Form.Text)
            , value (Maybe.withDefault "" email.value)  
            ] [] 
          ]
        , div [] [ Html.text (errorMessage email) ]
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
          [ button [ type_ "submit" ] [ text (if disabled then "Please wait" else "Send comment") ] 
          ]
        ]
      ]

    |> Html.form [ onSubmit Form.Submit ]
    |> Html.map toMsg

showPostPageCommentsView : List Comment -> (ShowPostPageMsg -> msg) -> Html msg
showPostPageCommentsView comments toMsg =
  let
      commentItem comment =
        div [] 
          [ div [] [ text comment.email ]
          , div [] [ p [] [ text comment.body ] ]
          , hr [] [] 
          ]
   in
      if List.isEmpty comments
          then 
            p [] [ text "No comments" ]
          else
            div [] (List.map commentItem comments)

showPostPageView : ShowPostPageState -> (ShowPostPageMsg -> msg) -> Html msg
showPostPageView { post, commentForm } toMsg = 
  if Requested == post.resource || commentForm.disabled
      then
        div [] [ text "Loading..." ]
      else
       case post.resource of
         NotRequested ->
           div [] []
         Requested ->
           div [] [ text "Loading" ]
         Error error ->
           div [] [ text "error" ]
         Available post_ ->
           div [] 
             [ h2 [] [ text post_.title ]
             , p [] [ text post_.body ]
             , h4 [] [ text "Comments" ]
             , showPostPageCommentsView post_.comments toMsg
             , h4 [] [ text "Post a comment" ]
             , newPostPageCommentFormView commentForm (toMsg << ShowPostPageCommentFormMsg)
             ]

--

type alias LoginForm =
  { username : String
  , password : String 
  }

loginFormValidate : Validation Never LoginForm
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
  , formModel : FormModel Never LoginForm 
  }

loginPageInApi : In LoginPageState (ApiModel Session) msg a
loginPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

loginPageInForm : In LoginPageState (FormModel Never LoginForm) msg a
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

loginPageHandleSubmit : (LoginPageMsg -> msg) -> LoginForm -> LoginPageState -> Update LoginPageState msg a
loginPageHandleSubmit toMsg form = 
  let json = form |> loginFormToJson |> Http.jsonBody 
   in loginPageInApi (apiSendRequest "" (Just json) (toMsg << LoginPageApiMsg))

loginPageUpdate : { onAuthResponse : Maybe Session -> a } -> LoginPageMsg -> (LoginPageMsg -> msg) -> LoginPageState -> Update LoginPageState msg a
loginPageUpdate { onAuthResponse } msg toMsg =

  let 
      handleApiResponse maybeSession = 
        loginPageInForm (formReset []) 
          >> andInvokeHandler (onAuthResponse maybeSession)

   in case msg of
     LoginPageApiMsg apiMsg -> 
       loginPageInApi (apiUpdate { onSuccess = handleApiResponse << Just, onError = handleApiResponse Nothing |> always } apiMsg (toMsg << LoginPageApiMsg))
     LoginFormMsg formMsg ->
       loginPageInForm (formUpdate { onSubmit = loginPageHandleSubmit toMsg } formMsg)

loginPageSubscriptions : LoginPageState -> (LoginPageMsg -> msg) -> Sub msg
loginPageSubscriptions state toMsg = Sub.none

loginPageFormView : FormModel Never LoginForm -> (Form.Msg -> msg) -> Html msg
loginPageFormView { form, disabled } toMsg =

  let username = form |> Form.getFieldAsString "username"
      password = form |> Form.getFieldAsString "password"

      inputField attributes field = input 
        ( [ onFocus (Form.Focus field.path)
          , onBlur (Form.Blur field.path)
          , onInput (String >> Form.Input field.path Form.Text)
          , value (Maybe.withDefault "" field.value)
          ] ++ attributes ) []

      alwaysEmpty = always ""

      errorMessage field = 
        field.liveError 
          |> Maybe.unpack alwaysEmpty (errorToString alwaysEmpty)

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

type RegisterFormError
  = PasswordConfirmationMismatch
  | MustAgreeWithTerms

registerFormErrorToString : RegisterFormError -> String
registerFormErrorToString error =
  case error of
    PasswordConfirmationMismatch ->
      "Password confirmation doesn’t match password"
    MustAgreeWithTerms ->
      "You must agree with the terms of this service to complete the registration"

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

validatePasswordConfirmation : Field -> Result (Error RegisterFormError) String
validatePasswordConfirmation = 

  let match password confirmation = 
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

validateChecked : Field -> Result (Error RegisterFormError) Bool
validateChecked = 

  let 
      mustBeChecked checked =
        if checked
            then Validate.succeed True
            else Validate.fail (Validate.customError MustAgreeWithTerms)
   in
      Validate.bool 
        |> Validate.andThen mustBeChecked

registerFormValidate : Validation RegisterFormError RegisterForm
registerFormValidate =
  succeed RegisterForm
    |> Validate.andMap (field "name" validateStringNonEmpty)
    |> Validate.andMap (field "email" validateEmail)
    |> Validate.andMap (field "username" validateStringNonEmpty)
    |> Validate.andMap (field "phoneNumber" validateStringNonEmpty)
    |> Validate.andMap (field "password" validatePassword)
    |> Validate.andMap validatePasswordConfirmation
    |> Validate.andMap (field "agreeWithTerms" validateChecked)

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
  , formModel : FormModel RegisterFormError RegisterForm 
  , usernames : Dict String Bool 
  , usernameStatus : UsernameStatus
  }

saveUsernameStatus : String -> Bool -> RegisterPageState -> Update RegisterPageState msg a
saveUsernameStatus username available state = save { state | usernames = Dict.insert username available state.usernames }

registerPageInApi : In RegisterPageState (ApiModel User) msg a
registerPageInApi =
    inState { get = .api, set = \state api -> { state | api = api } }

registerPageInForm : In RegisterPageState (FormModel RegisterFormError RegisterForm) msg a
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

registerPageHandleSubmit : (RegisterPageMsg -> msg) -> RegisterForm -> RegisterPageState -> Update RegisterPageState msg a
registerPageHandleSubmit toMsg form =
  let json = form |> registerFormToJson |> Http.jsonBody 
   in registerPageInApi (apiSendRequest "" (Just json) (toMsg << RegisterPageApiMsg))

registerPageUpdate : RegisterPageMsg -> (RegisterPageMsg -> msg) -> RegisterPageState -> Update RegisterPageState msg a
registerPageUpdate msg toMsg state = 
  case msg of
    RegisterPageApiMsg apiMsg ->
      state
        |> registerPageInApi (apiUpdate { onSuccess = always save, onError = always save } apiMsg (toMsg << RegisterPageApiMsg))
    RegisterFormMsg formMsg ->
      state
        |> registerPageInForm (formUpdate { onSubmit = registerPageHandleSubmit toMsg } formMsg)
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

registerPageFormView : FormModel RegisterFormError RegisterForm -> UsernameStatus -> (Form.Msg -> msg) -> Html msg
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
          |> Maybe.unpack (always "") (errorToString registerFormErrorToString)

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
        , div [] [ Html.text (errorMessage agreeWithTerms) ]
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

type alias RouterState route =
 { route : Maybe route
 , key   : Navigation.Key
 , fromUrl : Url -> Maybe route
 }

setRoute : Maybe route -> RouterState route -> Update (RouterState route) msg a
setRoute route state = save { state | route = route }

routerInit : (Url -> Maybe route) -> Navigation.Key -> (RouterMsg -> msg) -> Update (RouterState route) msg a
routerInit fromUrl_ key toMsg = 
  save RouterState
    |> andMap (save Nothing)
    |> andMap (save key)
    |> andMap (save fromUrl_)

routerRedirect : String -> RouterState route -> Update (RouterState route) msg a
routerRedirect href state = 
  state
    |> addCmd (Navigation.replaceUrl state.key href)

routerUpdate : { onRouteChange : Url -> Maybe route -> a } -> RouterMsg -> RouterState route -> Update (RouterState route) msg a
routerUpdate { onRouteChange } msg state = 
  case msg of
    UrlChange url ->
      let route = state.fromUrl url
       in state
        |> setRoute route
        |> andInvokeHandler (onRouteChange url route)
    UrlRequest (Browser.Internal url) ->
      state
        |> addCmd (Navigation.pushUrl state.key (Url.toString url))
    UrlRequest (Browser.External "") ->
      state
        |> save
    UrlRequest (Browser.External href) ->
      state
        |> addCmd (Navigation.load href)

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

current : Page -> { isHomePage : Bool, isNewPostPage : Bool, isShowPostPage : Bool, isLoginPage : Bool, isRegisterPage : Bool, isAboutPage : Bool, isNotFoundPage : Bool }
current page =
  
  let 
      default = { isHomePage = False, isNewPostPage = False, isShowPostPage = False, isLoginPage = False, isRegisterPage = False, isAboutPage = False, isNotFoundPage = False }
   in 
      case page of
        HomePage _ ->
          { default | isHomePage = True }
        NewPostPage _ ->
          { default | isNewPostPage = True }
        ShowPostPage _ ->
          { default | isShowPostPage = True }
        LoginPage _ ->
          { default | isLoginPage = True }
        RegisterPage _ ->
          { default | isRegisterPage = True }
        AboutPage ->
          { default | isAboutPage = True }
        NotFoundPage ->
          { default | isNotFoundPage = True }

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
            |> showPostPageUpdate showPostPageMsg (toMsg << ShowPostPageMsg)
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
  | UiMsg UiMsg

type alias State =
  { session : Maybe Session 
  , router : RouterState Route
  , ui : UiState
  , restrictedUrl : Maybe String
  , page : Page
  }

setRestrictedUrl : Maybe String -> State -> Update State msg a
setRestrictedUrl url state = save { state | restrictedUrl = url }

setSession : Maybe Session -> State -> Update State msg a
setSession session state = save { state | session = session }

inRouter : In State (RouterState Route) msg a
inRouter =
    inState { get = .router, set = \state router -> { state | router = router } }

inUi : In State UiState msg a
inUi =
    inState { get = .ui, set = \state ui -> { state | ui = ui } }

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
    |> andMap (routerInit fromUrl key RouterMsg)
    |> andMap uiInit
    |> andMap (save Nothing)
    |> andMap (save NotFoundPage)
    |> andThen (update (RouterMsg (UrlChange url)))

redirect : String -> State -> Update State msg a
redirect = inRouter << routerRedirect

loadPage : Update Page msg (State -> Update State msg a) -> State -> Update State msg a
loadPage update_ state = 
  let 
      isLoginRoute = always (Just Login == state.router.route)
   in 
      state
        |> inPage (always update_)
        |> andThenIf (not << isLoginRoute) (setRestrictedUrl Nothing)

handleRouteChange : Url -> Maybe Route -> State -> Update State PageMsg a
handleRouteChange url maybeRoute =

  let 
      ifAuthenticated gotoPage ({ session } as state) =
        if Nothing == session 
            then 
              state
                |> setRestrictedUrl (Just url.path)  -- Redirect back here after successful login
                |> andThen (redirect "/login")
            else 
              state
                |> gotoPage

      unlessAuthenticated gotoPage ({ session } as state) =
        state 
          |> if Nothing /= session then redirect "/" else gotoPage

   in 
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
          showPostPageInit id ShowPostPageMsg
            |> andThen (showPostPageUpdate FetchPost ShowPostPageMsg)
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

returnToRestrictedUrl : State -> Update State Msg a
returnToRestrictedUrl ({ restrictedUrl } as state) =
  redirect (Maybe.withDefault "/" restrictedUrl) state

handleAuthResponse : Maybe Session -> State -> Update State Msg a
handleAuthResponse maybeSession = 
  let 
      authenticated = always (Maybe.isJust maybeSession)
   in 
      setSession maybeSession
        >> andThen (updateSessionStorage maybeSession)
        >> andThenIf authenticated returnToRestrictedUrl

update : Msg -> State -> Update State Msg a
update msg =
  case msg of
    RouterMsg routerMsg ->
      inRouter (routerUpdate { onRouteChange = \url route -> mapCmd PageMsg << handleRouteChange url route } routerMsg)
    PageMsg pageMsg ->
      inPage (pageUpdate { onAuthResponse = handleAuthResponse, onPostAdded = always (redirect "/") } pageMsg PageMsg)
    UiMsg uiMsg ->
      inUi (uiUpdate uiMsg UiMsg)

subscriptions : State -> Sub Msg
subscriptions { page } = pageSubscriptions page PageMsg

view : State -> Document Msg
view ({ page } as state) = 
  { title = ""
  , body =
    [ Bulma.stylesheet
    , myNavbar state.page state.ui UiMsg
    , pageView page PageMsg 
    , div [] 
      [ a [ href "/" ] [ text "Home" ]
      , a [ href "/login" ] [ text "Login" ] 
      , a [ href "/logout" ] [ text "Logout" ]
      , a [ href "/register" ] [ text "Register" ]
      , a [ href "/about" ] [ text "About" ]
      , a [ href "/posts/new" ] [ text "New post" ]
--      , Html.text (Debug.toString state)
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
