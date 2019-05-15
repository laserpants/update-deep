module Main exposing (..)

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Form exposing (Form)
import Form.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (emptyBody)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode exposing (object)
import UiFormView
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, parse, oneOf, (</>))

--
--
--

type alias Update m a c = ( m, Cmd c, List a )

save : m -> Update m a c
save model = ( model, Cmd.none, [] )

runCmd : Cmd c -> m -> Update m a c
runCmd cmd model = ( model, cmd, [] )

mapCmd : (c -> d) -> Update m a c -> Update m a d
mapCmd f ( model, cmd, events ) = ( model, Cmd.map f cmd, events )

invokeHandler : a -> m -> Update m a c
invokeHandler handler model = ( model, Cmd.none, [ handler ] )

ap : Update (m -> n) a c -> Update m a c -> Update n a c
ap ( f, cmda, e ) ( model, cmdb, e2 ) = ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

map : (m -> n) -> Update m a c -> Update n a c
map f ( model, cmd, events ) = ( f model, cmd, events )

map2 : (m -> n -> o) -> Update m a c -> Update n a c -> Update o a c
map2 f = ap << map f

map3 : (m -> n -> o -> p) -> Update m a c -> Update n a c -> Update o a c -> Update p a c
map3 f x = ap << map2 f x

map4 : (m -> n -> o -> p -> q) -> Update m a c -> Update n a c -> Update o a c -> Update p a c -> Update q a c
map4 f x y = ap << map3 f x y

join : Update (Update m a c) a c -> Update m a c
join ( ( model, cmda, e ), cmdb, e2 ) = ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

andThen : (m -> Update n a c) -> Update m a c -> Update n a c
andThen f = join << map f

kleisli : (n -> Update o a c) -> (m -> Update n a c) -> m -> Update o a c
kleisli f g = andThen f << g

andRunCmd : Cmd c -> Update m a c -> Update m a c
andRunCmd = andThen << runCmd

andInvokeHandler : a -> Update m a c -> Update m a c
andInvokeHandler = andThen << invokeHandler

foldEvents : Update m (m -> Update m a c) c -> Update m a c
foldEvents ( model, cmd, events ) = List.foldr andThen ( model, cmd, [] ) events

message_ : ((n -> Update n b d) -> d) -> { get : n -> m , set : n -> m -> n , update : c -> m -> Update m (n -> Update n b d) c } -> c -> n -> Update n b d
message_ cons { update, get, set } msg model =
  model
    |> get
    |> update msg
    |> mapCmd (message cons { update = update, get = get, set = set })
    |> map (set model)
    |> foldEvents

message : ((n -> Update n b c) -> c) -> { update : a -> m -> Update m (n -> Update n b c) a, get : n -> m, set : n -> m -> n } -> a -> c
message cons access = cons << message_ cons access

applicationInit : (d -> e -> f -> Update m a c) -> d -> e -> f -> ( m, Cmd c )
applicationInit f a b c = let ( model, cmd, _ ) = f a b c in ( model, cmd )

documentInit : (f -> Update m a c) -> f -> ( m, Cmd c )
documentInit f a = let ( model, cmd, _ ) = f a in ( model, cmd )

runUpdate : (d -> e -> Update m a c) -> d -> e -> ( m, Cmd c )
runUpdate f a b = let ( model, cmd, _ ) = f a b in ( model, cmd )

--

application config =
  Browser.application
    { init          = applicationInit config.init
    , update        = runUpdate config.update
    , subscriptions = config.subscriptions
    , view          = config.view
    , onUrlChange   = config.onUrlChange
    , onUrlRequest  = config.onUrlRequest }

document config =
  Browser.document
    { init          = documentInit config.init
    , update        = runUpdate config.update
    , subscriptions = config.subscriptions
    , view          = config.view }

--
--
--

type ApiMsg a
  = Request String (Maybe Http.Body)
  | Response (Result Http.Error a)
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

setResource : ApiResource a -> ApiModel a -> Update (ApiModel a) b (ApiMsg a)
setResource resource state = save { state | resource = resource }

type HttpMethod
  = HttpGet
  | HttpPost

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

apiInit : RequestConfig a -> Update (ApiModel a) b (ApiMsg a)
apiInit { endpoint, method, decoder } =
  let expect = Http.expectJson Response decoder
      request suffix body =
        case method of
          HttpGet ->
            Http.get
              { url    = endpoint ++ suffix
              , expect = expect }
          HttpPost ->
            Http.post
              { url    = endpoint ++ suffix
              , expect = expect
              , body   = Maybe.withDefault emptyBody body }
   in save
    { resource = NotRequested
    , request  = request }

--apiDefaultHandlers : { onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e }
apiDefaultHandlers = { onSuccess = always save, onError = always save }

--apiUpdate : { onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e } -> ApiMsg b -> ApiModel b -> Update (ApiModel b) (a -> Update a c e) (ApiMsg b)
apiUpdate { onSuccess, onError } msg model =
  case msg of
    Request url maybeBody ->
      model
        |> setResource Requested
        |> andRunCmd (model.request url maybeBody)
    Response (Ok resource) ->
      model
        |> setResource (Available resource)
        |> andInvokeHandler (onSuccess resource)
    Response (Err error) ->
      model
        |> setResource (Error error)
        |> andInvokeHandler (onError error)
    Reset ->
      model
        |> setResource NotRequested

apiJsonRequest : String -> Value -> ApiMsg a
apiJsonRequest url = Request url << Just << Http.jsonBody

--

type FormMsg a
  = OnChange (Form.View.Model a)
  | ResetForm
  | SubmitForm a

type alias FormModel a =
  { state   : Form.View.Model a
  , form    : Form a (FormMsg a)
  , initial : a }

type alias Fields a = Form a (FormMsg a)

formInit : Form a (FormMsg a) -> a -> Update (FormModel a) b (FormMsg a)
formInit form values =
  save
    { state   = Form.View.idle values
    , form    = form
    , initial = values }

--formUpdate : { onSubmit : b -> a -> Update a c e } -> FormMsg b -> FormModel b -> Update (FormModel b) (a -> Update a c e) (FormMsg b)
formUpdate { onSubmit } msg model =
  case msg of
    OnChange formViewModel ->
      save { model | state = formViewModel }
    ResetForm ->
      save { model | state = Form.View.idle model.initial }
    SubmitForm values ->
      let { state } = model
       in save { model | state = { state | state = Form.View.Loading } }
        |> andInvokeHandler (onSubmit values)

formView : FormModel a -> Html (FormMsg a)
formView { form, state } =
  UiFormView.view
    { onChange   = OnChange
    , action     = "Submit"
    , loading    = "Submit"
    , validation = Form.View.ValidateOnSubmit
    } form state

--

type alias DataUser =
  { id    : Int
  , email : String
  , login : String
  , name  : String }

dataUserDecoder : Json.Decoder DataUser
dataUserDecoder =
  Json.map4 DataUser
    (Json.field "id"    Json.int)
    (Json.field "email" Json.string)
    (Json.field "login" Json.string)
    (Json.field "name"  Json.string)

--

type alias DataPost =
  { id    : Int
  , title : String
  , body  : String }

dataPostDecoder : Json.Decoder DataPost
dataPostDecoder =
  Json.map3 DataPost
    (Json.field "id"    Json.int)
    (Json.field "title" Json.string)
    (Json.field "body"  Json.string)

--

type alias DataComment =
  { id    : Int
  , email : String
  , body  : String }

dataCommentDecoder : Json.Decoder DataComment
dataCommentDecoder =
  Json.map3 DataComment
    (Json.field "id"    Json.int)
    (Json.field "email" Json.string)
    (Json.field "body"  Json.string)

--

type alias AuthRegisterForm =
  { login    : String
  , password : String }

authRegisterFormFields : Fields AuthRegisterForm
authRegisterFormFields =

  let loginField =
        Form.textField
          { parser = Ok
          , value  = .login
          , update = \value values -> { values | login = value }
          , attributes =
            { label       = "Email"
            , placeholder = "Email" } }

      passwordField =
        Form.passwordField
          { parser = Ok
          , value  = .password
          , update = \value values -> { values | password = value }
          , attributes =
            { label       = "Password"
            , placeholder = "Your password" } }

   in Form.succeed AuthRegisterForm
    |> Form.append loginField
    |> Form.append passwordField
    |> Form.map SubmitForm

authRegisterFormToJson : AuthRegisterForm -> Value
authRegisterFormToJson { login, password } =
  object [ ( "login"    , Encode.string login )
         , ( "password" , Encode.string password ) ]

--

type alias AuthLoginForm =
  { login    : String
  , password : String }

authLoginFormFields : Fields AuthLoginForm
authLoginFormFields =

  let loginField =
        Form.textField
          { parser = Ok
          , value  = .login
          , update = \value values -> { values | login = value }
          , attributes =
            { label       = "Login"
            , placeholder = "Login" } }

      passwordField =
        Form.passwordField
          { parser = Ok
          , value  = .password
          , update = \value values -> { values | password = value }
          , attributes =
            { label       = "Password"
            , placeholder = "Your password" } }

   in Form.succeed AuthLoginForm
    |> Form.append loginField
    |> Form.append passwordField
    |> Form.map SubmitForm

authLoginFormToJson : AuthLoginForm -> Value
authLoginFormToJson { login, password } =
  object [ ( "login"    , Encode.string login )
         , ( "password" , Encode.string password ) ]

--

type alias CommentsCreateForm =
  { email   : String
  , comment : String }

commentsCreateFormFields : Fields CommentsCreateForm
commentsCreateFormFields =

  let emailField =
        Form.textField
          { parser = Ok
          , value  = .email
          , update = \value values -> { values | email = value }
          , attributes =
            { label       = "Email"
            , placeholder = "Email" } }

      commentField =
        Form.textareaField
          { parser = Ok
          , value  = .comment
          , update = \value values -> { values | comment = value }
          , attributes =
            { label       = "Comment"
            , placeholder = "Comment" } }

   in Form.succeed CommentsCreateForm
    |> Form.append emailField
    |> Form.append commentField
    |> Form.map SubmitForm

commentsCreateFormToJson : CommentsCreateForm -> Value
commentsCreateFormToJson { email, comment } =
  object [ ( "email"   , Encode.string email )
         , ( "comment" , Encode.string comment ) ]

--

type alias PostsCreateForm =
  { title : String
  , body  : String }

postsCreateFormFields : Fields PostsCreateForm
postsCreateFormFields =

  let titleField =
        Form.textField
          { parser = Ok
          , value  = .title
          , update = \value values -> { values | title = value }
          , attributes =
            { label       = "Title"
            , placeholder = "Title" } }

      bodyField =
        Form.textareaField
          { parser = Ok
          , value  = .body
          , update = \value values -> { values | body = value }
          , attributes =
            { label       = "Body"
            , placeholder = "Body" } }

   in Form.succeed PostsCreateForm
    |> Form.append titleField
    |> Form.append bodyField
    |> Form.map SubmitForm

postsCreateFormToJson : PostsCreateForm -> Value
postsCreateFormToJson { title, body } =
  object [ ( "title" , Encode.string title )
         , ( "body"  , Encode.string body ) ]

--

type Route
  = Home
  | About
  | PostsCreate
  | Post Int
  | CommentPost Int
  | Login
  | Register

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home        (Parser.top)
    , Parser.map About       (Parser.s "about")
    , Parser.map PostsCreate (Parser.s "posts" </> Parser.s "new")
    , Parser.map Post        (Parser.s "posts" </> Parser.int)
    , Parser.map CommentPost (Parser.s "posts" </> Parser.int </> Parser.s "comment")
    , Parser.map Login       (Parser.s "login")
    , Parser.map Register    (Parser.s "register") ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser

--

type alias RouterUpdate a = RouterModel -> Update RouterModel a (RouterMsg a)

type RouterMsg a
  = UrlChange Url
  | UrlRequest UrlRequest
  | Redirect String

type alias RouterModel =
  { route : Maybe Route
  , key   : Navigation.Key }

setRoute : Maybe Route -> RouterModel -> Update RouterModel a (RouterMsg a)
setRoute route model = save { model | route = route }

routerInit : Navigation.Key -> Update RouterModel b (RouterMsg a)
routerInit key =
  save
    { route = Nothing
    , key   = key }

routerUpdate : { t | onRouteChange : Maybe Route -> a } -> RouterMsg a -> RouterUpdate a
routerUpdate { onRouteChange } msg model =
  case msg of
    UrlChange url ->
      let route = fromUrl url
       in model
        |> setRoute route
        |> andInvokeHandler (onRouteChange route)
    UrlRequest (Browser.Internal url) ->
      model
        |> runCmd (Navigation.pushUrl model.key (Url.toString url))
    UrlRequest (Browser.External href) ->
      model
        |> runCmd (Navigation.load href)
    Redirect href ->
      model
        |> runCmd (Navigation.replaceUrl model.key href)

routerSubscriptions : RouterModel -> Sub (RouterMsg a)
routerSubscriptions model = Sub.none

--

type alias UiUpdate a = UiModel -> Update UiModel a (UiMsg a)

type UiMsg a
  = NoUiMsg

type alias UiModel =
  {}

uiInit : Update UiModel b (UiMsg a)
uiInit = save {}

uiUpdate : UiMsg a -> UiModel -> Update UiModel a (UiMsg a)
uiUpdate msg model = save model

uiSubscriptions : UiModel -> Sub (UiMsg a)
uiSubscriptions model = Sub.none

--

type alias AuthLoginUpdate a = AuthLoginModel -> Update AuthLoginModel a (AuthLoginMsg a)

type AuthLoginMsg a
  = AuthLoginModelMsg (AuthLoginUpdate a)

type alias AuthLoginModel =
  { user : ApiModel DataUser
  , form : FormModel AuthLoginForm }

authLoginInit : Update AuthLoginModel b (AuthLoginMsg a)
authLoginInit =
  let api = apiInit { endpoint = "/auth/login"
                    , method   = HttpPost
                    , decoder  = Json.field "user" dataUserDecoder }
      form = formInit authLoginFormFields { login = "", password = "" }
   in map2 AuthLoginModel
        (api  |> mapCmd authLoginApiMsg)
        (form |> mapCmd authLoginFormMsg)

authLoginApiMsg : ApiMsg DataUser -> AuthLoginMsg a
authLoginApiMsg = message AuthLoginModelMsg
  { update = apiUpdate { onSuccess = always save, onError = always save }
  , get = .user
  , set = \model user -> { model | user = user } }

authLoginFormMsg : FormMsg AuthLoginForm -> AuthLoginMsg a
authLoginFormMsg = message AuthLoginModelMsg
  { update = formUpdate { onSubmit = always save }
  , get = .form
  , set = \model form -> { model | form = form } }

authLoginUpdate : AuthLoginMsg a -> AuthLoginModel -> Update AuthLoginModel a (AuthLoginMsg a)
authLoginUpdate msg model =
  case msg of
    AuthLoginModelMsg update ->
      update model

authLoginSubscriptions : AuthLoginModel -> Sub (AuthLoginMsg a)
authLoginSubscriptions model = Sub.none

authLoginView : AuthLoginModel -> Html (AuthLoginMsg a)
authLoginView { form } =
  Html.map authLoginFormMsg (formView form)

--

type alias AuthRegisterUpdate a = AuthRegisterModel -> Update AuthRegisterModel a (AuthRegisterMsg a)

type AuthRegisterMsg a
  = AuthRegisterModelMsg (AuthRegisterUpdate a)

type alias AuthRegisterModel =
  { response : ApiModel { status : String }
  , form     : FormModel AuthRegisterForm }

authRegisterInit : Update AuthRegisterModel b (AuthRegisterMsg a)
authRegisterInit =
  let decoder = Json.field "status" Json.string |> Json.map (\status -> { status = status })
      api = apiInit { endpoint = "/auth/register"
                    , method   = HttpPost
                    , decoder  = decoder }
      form = formInit authRegisterFormFields { login = "", password = "" }
   in map2 AuthRegisterModel
        (api  |> foldEvents |> mapCmd authRegisterApiMsg)
        (form |> foldEvents |> mapCmd authRegisterFormMsg)

authRegisterApiMsg : ApiMsg { status : String } -> AuthRegisterMsg a
authRegisterApiMsg = message AuthRegisterModelMsg
  { update = apiUpdate { onSuccess = always save, onError = always save }
  , get = .response
  , set = \model response -> { model | response = response } }

authRegisterFormMsg : FormMsg AuthRegisterForm -> AuthRegisterMsg a
authRegisterFormMsg = message AuthRegisterModelMsg
  { update = formUpdate { onSubmit = always save }
  , get = .form
  , set = \model form -> { model | form = form } }

authRegisterUpdate : AuthRegisterMsg a -> AuthRegisterModel -> Update AuthRegisterModel a (AuthRegisterMsg a)
authRegisterUpdate msg model =
  case msg of
    AuthRegisterModelMsg update ->
      update model

authRegisterSubscriptions : AuthRegisterModel -> Sub (AuthRegisterMsg a)
authRegisterSubscriptions model = Sub.none

authRegisterView : AuthRegisterModel -> Html (AuthRegisterMsg a)
authRegisterView { form } =
  Html.map authRegisterFormMsg (formView form)

--

type alias PostsShowUpdate a = PostsShowModel -> Update PostsShowModel a (PostsShowMsg a)

type PostsShowMsg a
  = NoPostsShowMsg

type alias PostsShowModel =
  {}

postsShowInit : Update PostsShowModel b (PostsShowMsg a)
postsShowInit = save {}

postsShowUpdate : PostsShowMsg a -> PostsShowModel -> Update PostsShowModel a (PostsShowMsg a)
postsShowUpdate msg model =
  case msg of
    _ -> save model

postsShowSubscriptions : PostsShowModel -> Sub (PostsShowMsg a)
postsShowSubscriptions model = Sub.none

--

type alias PostsCommentUpdate a = PostsCommentModel -> Update PostsCommentModel a (PostsCommentMsg a)

type PostsCommentMsg a
  = NoPostsCommentMsg

type alias PostsCommentModel =
  {}

postsCommentsInit : Update PostsCommentModel b (PostsCommentMsg a)
postsCommentsInit = save {}

postsCommentsUpdate : PostsCommentMsg a -> PostsCommentModel -> Update PostsCommentModel a (PostsCommentMsg a)
postsCommentsUpdate msg model =
  case msg of
    _ -> save model

postsCommentsSubscriptions : PostsCommentModel -> Sub (PostsCommentMsg a)
postsCommentsSubscriptions model = Sub.none

--

type alias HomePageUpdate a = HomePageModel -> Update HomePageModel a (HomePageMsg a)

type HomePageMsg a
  = HomePageModelMsg (HomePageUpdate a)

type alias HomePageModel =
  { collection : ApiModel (List DataPost) }

homePageInit : Update HomePageModel b (HomePageMsg a)
homePageInit =
   let collection = apiInit { endpoint = "/posts"
                            , method   = HttpGet
                            , decoder  = Json.field "posts" (Json.list dataPostDecoder) }
    in map HomePageModel
         (collection |> foldEvents |> mapCmd homePageApiMsg)

homePageApiMsg : ApiMsg (List DataPost) -> HomePageMsg a
homePageApiMsg = message HomePageModelMsg
  { update = apiUpdate { onSuccess = always save, onError = always save }
  , get = .collection
  , set = \model collection -> { model | collection = collection } }

homePageUpdate : HomePageMsg a -> HomePageModel -> Update HomePageModel a (HomePageMsg a)
homePageUpdate msg model =
  case msg of
    HomePageModelMsg update ->
      update model

homePageSubscriptions : HomePageModel -> Sub (HomePageMsg a)
homePageSubscriptions model = Sub.none

fetchButton =
  div [] [ button [ onClick (homePageApiMsg (Request "" Nothing)) ] [ text "Fetch" ] ]

item post = 
  div [] 
    [ h1 [] [ text post.title ] 
    , p [] [ text post.body ] ]

homePageView : HomePageModel -> Html (HomePageMsg a)
homePageView model =
  case model.collection.resource of
    NotRequested ->
      div [] 
        [ text "Not requested" 
        , fetchButton ]
    Requested ->
      text "Requested"
    Error error ->
      div [] 
        [ text "Error" 
        , fetchButton ]
    Available posts ->
      div [] (List.map item posts)

--

type alias PostsCreateUpdate a = PostsCreateModel -> Update PostsCreateModel a (PostsCreateMsg a)

type PostsCreateMsg a
  = PostsCreateModelMsg (PostsCreateUpdate a)

type alias PostsCreateModel =
  { post : ApiModel DataPost
  , form : FormModel PostsCreateForm }

postsCreateInit : Update PostsCreateModel b (PostsCreateMsg a)
postsCreateInit =
  let api = apiInit { endpoint = "/posts"
                    , method   = HttpPost
                    , decoder  = Json.field "post" dataPostDecoder }
      form = formInit postsCreateFormFields { title = "", body = "" }
   in map2 PostsCreateModel
        (api  |> foldEvents |> mapCmd postsCreateApiMsg)
        (form |> foldEvents |> mapCmd postsCreateFormMsg)

postsCreateApiMsg : ApiMsg DataPost -> PostsCreateMsg a
postsCreateApiMsg = message PostsCreateModelMsg
  { update = apiUpdate { onSuccess = always save, onError = always save }
  , get = .post
  , set = \model post -> { model | post = post } }

postsCreateHandleSubmit : PostsCreateForm -> PostsCreateUpdate a
postsCreateHandleSubmit form = 
  postsCreateApiMsg (apiJsonRequest "" (postsCreateFormToJson form))
    |> postsCreateUpdate 

postsCreateFormMsg : FormMsg PostsCreateForm -> PostsCreateMsg a
postsCreateFormMsg = message PostsCreateModelMsg
  { update = formUpdate { onSubmit = postsCreateHandleSubmit }
  , get = .form
  , set = \model form -> { model | form = form } }

postsCreateUpdate : PostsCreateMsg a -> PostsCreateModel -> Update PostsCreateModel a (PostsCreateMsg a)
postsCreateUpdate msg model =
  case msg of
    PostsCreateModelMsg update ->
      update model

postsCreateSubscriptions : PostsCreateModel -> Sub (PostsCreateMsg a)
postsCreateSubscriptions model = Sub.none

postsCreateView : PostsCreateModel -> Html (PostsCreateMsg a)
postsCreateView { form } =
  Html.map postsCreateFormMsg (formView form)

--

type alias Flags = ()

--

type alias PageUpdate a = Page -> Update Page a (PageMsg a)

type PageMsg a
  = PageModelMsg (PageUpdate a)
  | HomePageMsg (HomePageUpdate a)
  | AuthLoginMsg (AuthLoginUpdate a)
  | AuthRegisterMsg (AuthRegisterUpdate a)
  | PostsCreateMsg (PostsCreateUpdate a)
  | PostsShowMsg (PostsShowUpdate a)
  | PostsCommentMsg (PostsCommentUpdate a)
  | SetPage Page

type Page
  = HomePage HomePageModel
  | AboutPage
  | PostsCreatePage PostsCreateModel
  | PostsShowPage PostsShowModel
  | PostsCommentPage PostsCommentModel
  | LoginPage AuthLoginModel
  | RegisterPage AuthRegisterModel

pageInit : Update Page b (PageMsg a)
pageInit = 
  map HomePage homePageInit
    |> mapCmd homePageMsg
    |> foldEvents

loginPageMsg : AuthLoginMsg a -> PageMsg a
loginPageMsg = AuthLoginMsg << authLoginUpdate

registerPageMsg : AuthRegisterMsg a -> PageMsg a
registerPageMsg = AuthRegisterMsg << authRegisterUpdate

homePageMsg : HomePageMsg a -> PageMsg a
homePageMsg = HomePageMsg << homePageUpdate

postsCreatePageMsg : PostsCreateMsg a -> PageMsg a
postsCreatePageMsg = PostsCreateMsg << postsCreateUpdate

postsShowPageMsg : PostsShowMsg a -> PageMsg a
postsShowPageMsg = PostsShowMsg << postsShowUpdate

postsCommentsPageMsg : PostsCommentMsg a -> PageMsg a
postsCommentsPageMsg = PostsCommentMsg << postsCommentsUpdate

pageUpdate : PageMsg a -> Page -> Update Page a (PageMsg a)
pageUpdate msg page =
  case ( msg, page ) of
    ( HomePageMsg update, HomePage homePageModel ) ->
      update homePageModel
        |> map HomePage
        |> mapCmd homePageMsg
    ( AuthLoginMsg update, LoginPage authLoginModel ) ->
      update authLoginModel
        |> map LoginPage
        |> mapCmd loginPageMsg
    ( AuthRegisterMsg update, RegisterPage authRegisterModel ) ->
      update authRegisterModel
        |> map RegisterPage
        |> mapCmd registerPageMsg
    ( PostsCreateMsg update, PostsCreatePage postsCreateModel ) ->
      update postsCreateModel
        |> map PostsCreatePage
        |> mapCmd postsCreatePageMsg
    ( PostsShowMsg update, PostsShowPage postsShowModel ) ->
      update postsShowModel
        |> map PostsShowPage
        |> mapCmd postsShowPageMsg
    ( PostsCommentMsg update, PostsCommentPage postsCommentsModel ) ->
      update postsCommentsModel
        |> map PostsCommentPage
        |> mapCmd postsCommentsPageMsg
    ( PageModelMsg update, _ ) ->
      update page
    ( SetPage newPage, _ ) ->
      save newPage
    ( msg_, _ ) ->
      Debug.log ("Message not delivered: " ++ Debug.toString msg_) (save page)

pageSubscriptions : Page -> Sub (Msg a)
pageSubscriptions page =
  case page of
    HomePage homePageModel ->
      Sub.map appHomePageMsg (homePageSubscriptions homePageModel)
    AboutPage ->
      Sub.none
    PostsCreatePage postsCreateModel ->
      Sub.map appPostsCreateMsg (postsCreateSubscriptions postsCreateModel)
    PostsShowPage postsShowModel ->
      Sub.map appPostsShowMsg (postsShowSubscriptions postsShowModel)
    PostsCommentPage postsCommentsModel ->
      Sub.map appPostsCommentMsg (postsCommentsSubscriptions postsCommentsModel)
    LoginPage authLoginModel ->
      Sub.map appLoginMsg (authLoginSubscriptions authLoginModel)
    RegisterPage authRegisterModel ->
      Sub.map appRegisterMsg (authRegisterSubscriptions authRegisterModel)

pageView : Page -> Html (Msg a)
pageView page =
  case page of
    HomePage homePageModel ->
      Html.map appHomePageMsg (homePageView homePageModel)
    AboutPage ->
      text "about"
    PostsCreatePage postsCreateModel ->
      Html.map appPostsCreateMsg (postsCreateView postsCreateModel)
    PostsShowPage postsShowModel ->
      text "show post"
    PostsCommentPage postsCommentsModel ->
      text "comment post"
    LoginPage authLoginModel ->
      Html.map appLoginMsg (authLoginView authLoginModel)
    RegisterPage authRegisterModel ->
      Html.map appRegisterMsg (authRegisterView authRegisterModel)

--

type alias AppUpdate a = Model -> Update Model a (Msg a)

type Msg a
  = ModelMsg (AppUpdate a)

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : Page }

appInit : Flags -> Url -> Navigation.Key -> Update Model b (Msg a)
appInit flags url key =
  let router = routerInit key
      ui     = uiInit
      page   = pageInit
   in map3 Model
        (router |> foldEvents |> mapCmd routerMsg)
        (ui     |> foldEvents |> mapCmd uiMsg)
        (page   |> foldEvents |> mapCmd pageMsg)

routerMsg : RouterMsg (AppUpdate a) -> Msg a
routerMsg = message ModelMsg
  { update = routerUpdate { onRouteChange = handleRouteChange }
  , get = .router
  , set = \model router -> { model | router = router } }

uiMsg : UiMsg (AppUpdate a) -> Msg a
uiMsg = message ModelMsg
  { update = uiUpdate
  , get = .ui
  , set = \model ui -> { model | ui = ui } }

pageMsg : PageMsg (AppUpdate a) -> Msg a
pageMsg = message ModelMsg
  { update = pageUpdate
  , get = .page
  , set = \model page -> { model | page = page } }

appHomePageMsg : HomePageMsg (AppUpdate a) -> Msg a
appHomePageMsg = pageMsg << HomePageMsg << homePageUpdate

appPostsCreateMsg : PostsCreateMsg (AppUpdate a) -> Msg a
appPostsCreateMsg = pageMsg << PostsCreateMsg << postsCreateUpdate

appPostsShowMsg : PostsShowMsg (AppUpdate a) -> Msg a
appPostsShowMsg = pageMsg << PostsShowMsg << postsShowUpdate

appPostsCommentMsg : PostsCommentMsg (AppUpdate a) -> Msg a
appPostsCommentMsg = pageMsg << PostsCommentMsg << postsCommentsUpdate

appLoginMsg : AuthLoginMsg (AppUpdate a) -> Msg a
appLoginMsg = pageMsg << AuthLoginMsg << authLoginUpdate

appRegisterMsg : AuthRegisterMsg (AppUpdate a) -> Msg a
appRegisterMsg = pageMsg << AuthRegisterMsg << authRegisterUpdate

handleRouteChange : Maybe Route -> AppUpdate a
handleRouteChange route model =
  let updatePage msg = appUpdate (pageMsg msg) model
   in case route of
        Just Home ->
          map HomePage homePageInit
            |> mapCmd homePageMsg
            |> updatePage << PageModelMsg << always
        Just About ->
          updatePage (SetPage AboutPage)
        Just PostsCreate ->
          map PostsCreatePage postsCreateInit
            |> mapCmd postsCreatePageMsg
            |> updatePage << PageModelMsg << always
        Just (Post id) ->
          save model
        Just (CommentPost postId) ->
          save model
        Just Login ->
          map LoginPage authLoginInit
            |> mapCmd loginPageMsg
            |> updatePage << PageModelMsg << always
        Just Register ->
          map RegisterPage authRegisterInit
            |> mapCmd registerPageMsg
            |> updatePage << PageModelMsg << always
        Nothing ->
          save model

appUpdate : Msg a -> AppUpdate a
appUpdate msg model =
  case msg of
    ModelMsg update ->
      update model

subscriptions : Model -> Sub (Msg a)
subscriptions model =
  Sub.batch
    ( pageSubscriptions model.page ::
      [ Sub.map routerMsg (routerSubscriptions model.router)
      , Sub.map uiMsg (uiSubscriptions model.ui) ] )

view : Model -> Document (Msg a)
view model =
  { title = ""
  , body  = [
      div []
        [ ul []
          [ li [] [ a [ href "/" ] [ text "Home" ] ]
          , li [] [ a [ href "/about" ] [ text "About" ] ]
          , li [] [ a [ href "/login" ] [ text "Login" ] ]
          , li [] [ a [ href "/register" ] [ text "Register" ] ] 
          , li [] [ a [ href "/posts/new" ] [ text "Create post" ] ] ]
        , text (Debug.toString model)
        , hr [] []
        , pageView model.page
        ]
    ]
  }

onUrlChange : Url -> Msg a
onUrlChange url = routerMsg (UrlChange url)

onUrlRequest : UrlRequest -> Msg a
onUrlRequest urlRequest = routerMsg (UrlRequest urlRequest)

main : Program Flags Model (Msg a)
main =
  application
    { init          = appInit
    , update        = appUpdate
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = onUrlChange
    , onUrlRequest  = onUrlRequest }
