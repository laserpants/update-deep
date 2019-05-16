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

type alias Update m c e = ( m, Cmd c, List e )

save : m -> Update m c e
save model = ( model, Cmd.none, [] )

runCmd : Cmd c -> m -> Update m c e
runCmd cmd state = ( state, cmd, [] )

mapCmd : (c -> d) -> Update m c e -> Update m d e
mapCmd f ( model, cmd, events ) = ( model, Cmd.map f cmd, events )

invokeHandler : e -> m -> Update m c e
invokeHandler handler state = ( state, Cmd.none, [ handler ] )

ap : Update (a -> b) c e -> Update a c e -> Update b c e
ap ( f, cmda, e ) ( model, cmdb, e2 ) = ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

map : (a -> b) -> Update a c e -> Update b c e
map f ( model, cmd, events ) = ( f model, cmd, events )

map2 : (a -> b -> p) -> Update a c e -> Update b c e -> Update p c e
map2 f = ap << map f

map3 : (a -> b -> p -> q) -> Update a c e -> Update b c e -> Update p c e -> Update q c e
map3 f x = ap << map2 f x

map4 : (a -> b -> p -> q -> r) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e
map4 f x y = ap << map3 f x y

map5 : (a -> b -> p -> q -> r -> s) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e
map5 f x y z = ap << map4 f x y z

join : Update (Update a c e) c e -> Update a c e
join ( ( model, cmda, e ), cmdb, e2 ) = ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )

andThen : (b -> Update a c e) -> Update b c e -> Update a c e
andThen f = join << map f

kleisli : (b -> Update d c e) -> (a -> Update b c e) -> (a -> Update d c e)
kleisli f g = andThen f << g

andRunCmd : Cmd c -> Update a c e -> Update a c e
andRunCmd = andThen << runCmd

andInvokeHandler : e -> Update a c e -> Update a c e
andInvokeHandler = andThen << invokeHandler

foldEvents : Update a c (a -> Update a c e) -> Update a c e
foldEvents ( m, cmd, events ) = List.foldr andThen ( m, cmd, [] ) events

andWithEvents : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
andWithEvents setter = foldEvents << andThen setter

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

setResource : ApiResource a -> ApiModel a -> Update (ApiModel a) (ApiMsg a) e
setResource resource state = save { state | resource = resource }

type HttpMethod
  = HttpGet
  | HttpPost

type alias RequestConfig a =
  { endpoint : String
  , method   : HttpMethod
  , decoder  : Json.Decoder a }

apiInit : RequestConfig a -> Update (ApiModel a) (ApiMsg a) e
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

type alias ApiHandlers a b c e = 
  { onSuccess : b -> a -> Update a c e
  , onError   : Http.Error -> a -> Update a c e }

apiDefaultHandlers : ApiHandlers a b c e
apiDefaultHandlers = { onSuccess = always save, onError = always save }

apiUpdate : ApiHandlers a b c e -> ApiMsg b -> ApiModel b -> Update (ApiModel b) (ApiMsg b) (a -> Update a c e)
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
  = ChangeForm (Form.View.Model a)
  | ResetForm
  | SubmitForm a

type alias FormModel a =
  { state   : Form.View.Model a
  , form    : Form a (FormMsg a)
  , initial : a }

type alias Fields a = Form a (FormMsg a)

formInit : Form a (FormMsg a) -> a -> Update (FormModel a) (FormMsg a) e
formInit form values =
  save
    { state   = Form.View.idle values
    , form    = form
    , initial = values }

formUpdate : { onSubmit : b -> a -> Update a c e } -> FormMsg b -> FormModel b -> Update (FormModel b) (FormMsg b) (a -> Update a c e)
formUpdate { onSubmit } msg model =
  case msg of
    ChangeForm formViewModel ->
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
    { onChange   = ChangeForm
    , action     = "Submit"
    , loading    = "Submit"
    , validation = Form.View.ValidateOnSubmit
    } form state

--
--
--

type Route
 = Home
 | About
 | NewPost
 | Post Int
 | CommentPost Int
 | Login
 | Register

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home        (Parser.top)
    , Parser.map About       (Parser.s "about")
    , Parser.map Login       (Parser.s "login")
    , Parser.map Register    (Parser.s "register")
    , Parser.map NewPost     (Parser.s "posts" </> Parser.s "new")
    , Parser.map Post        (Parser.s "posts" </> Parser.int)
    , Parser.map CommentPost (Parser.s "posts" </> Parser.int </> Parser.s "comment") ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser

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

type PostsCommentCreateMsg
  = PostsCommentCreateApiPostMsg (ApiMsg DataPost)
  | PostsCommentCreateApiCommentMsg (ApiMsg DataComment)
  | PostsCommentCreateFormMsg (FormMsg CommentsCreateForm)

type alias PostsCommentCreateModel =
  { post    : ApiModel DataPost
  , comment : ApiModel DataComment
  , form    : FormModel CommentsCreateForm }

postsCommentCreateInsertAsPostIn : PostsCommentCreateModel -> ApiModel DataPost -> Update PostsCommentCreateModel PostsCommentCreateMsg a
postsCommentCreateInsertAsPostIn model post = save { model | post = post }

postsCommentCreateInsertAsCommentIn : PostsCommentCreateModel -> ApiModel DataComment -> Update PostsCommentCreateModel PostsCommentCreateMsg a
postsCommentCreateInsertAsCommentIn model comment = save { model | comment = comment }

postsCommentCreateInsertAsFormIn : PostsCommentCreateModel -> FormModel CommentsCreateForm -> Update PostsCommentCreateModel PostsCommentCreateMsg a
postsCommentCreateInsertAsFormIn model form = save { model | form = form }

postsCommentCreateInit : Int -> Update PostsCommentCreateModel PostsCommentCreateMsg a
postsCommentCreateInit postId =
  let post    = apiInit { endpoint = "/posts/" ++ String.fromInt postId
                        , method   = HttpGet
                        , decoder  = Json.field "post" dataPostDecoder }
      comment = apiInit { endpoint = "/posts/" ++ String.fromInt postId ++ "/comments"
                        , method   = HttpPost
                        , decoder  = Json.field "post" dataCommentDecoder }
      form = formInit commentsCreateFormFields { email = "", comment = "" }
   in map3 PostsCommentCreateModel
        (post    |> mapCmd PostsCommentCreateApiPostMsg)
        (comment |> mapCmd PostsCommentCreateApiCommentMsg)
        (form    |> mapCmd PostsCommentCreateFormMsg)

postsCommentCreateUpdate : PostsCommentCreateMsg -> PostsCommentCreateModel -> Update PostsCommentCreateModel PostsCommentCreateMsg a
postsCommentCreateUpdate msg model =
  case msg of
    PostsCommentCreateApiPostMsg apiMsg ->
      model.post
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd PostsCommentCreateApiPostMsg
        |> andWithEvents (postsCommentCreateInsertAsPostIn model)
    PostsCommentCreateApiCommentMsg apiMsg ->
      model.comment
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd PostsCommentCreateApiCommentMsg
        |> andWithEvents (postsCommentCreateInsertAsCommentIn model)
    PostsCommentCreateFormMsg formMsg ->
      model.form
        |> formUpdate { onSubmit = always save } formMsg
        |> mapCmd PostsCommentCreateFormMsg
        |> andWithEvents (postsCommentCreateInsertAsFormIn model)

postsCommentCreateSubscriptions : PostsCommentCreateModel -> Sub PostsCommentCreateMsg
postsCommentCreateSubscriptions model = Sub.none

postsCommentCreateView : PostsCommentCreateModel -> Html PostsCommentCreateMsg
postsCommentCreateView { post, form } =
  case post.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
    Requested ->
      div [] [ text "Requested..." ]
    Error error ->
      div [] [ text "error" ]
    Available item ->
      div [] [ h1 []
        [ text item.title
        , Html.map PostsCommentCreateFormMsg (formView form) ] ]

--

type PostsShowMsg
  = PostsShowApiMsg (ApiMsg DataPost)

type alias PostsShowModel =
  { post : ApiModel DataPost }

postsShowInsertAsPostIn : PostsShowModel -> ApiModel DataPost -> Update PostsShowModel PostsShowMsg a
postsShowInsertAsPostIn model post = save { model | post = post }

postsShowInit : Int -> Update PostsShowModel PostsShowMsg a
postsShowInit postId =
  let post = apiInit { endpoint = "/posts/" ++ String.fromInt postId
                     , method   = HttpGet
                     , decoder  = Json.field "post" dataPostDecoder }
   in map PostsShowModel
        (post |> mapCmd PostsShowApiMsg)

postsShowUpdate : PostsShowMsg -> PostsShowModel -> Update PostsShowModel PostsShowMsg a
postsShowUpdate msg model =
  case msg of
    PostsShowApiMsg apiMsg ->
      model.post
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd PostsShowApiMsg
        |> andWithEvents (postsShowInsertAsPostIn model)

postsShowSubscriptions : PostsShowModel -> Sub PostsShowMsg
postsShowSubscriptions model = Sub.none

postsShowView : PostsShowModel -> Html PostsShowMsg
postsShowView { post } =
  case post.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
    Requested ->
      div [] [ text "Requested..." ]
    Error error ->
      div [] [ text "error" ]
    Available item ->
      div [] [ h1 [] [ text item.title ] ]

--

type HomePageMsg
  = HomePageApiMsg (ApiMsg (List DataPost))
  | FetchPosts

type alias HomePageModel =
  { posts : ApiModel (List DataPost) }

insertAsPostsIn : HomePageModel -> ApiModel (List DataPost) -> Update HomePageModel HomePageMsg a
insertAsPostsIn model posts = save { model | posts = posts }

homePageInit : Maybe (List DataPost) -> Update HomePageModel HomePageMsg a
homePageInit maybePosts =
  let resource = 
        case maybePosts of
          Nothing -> NotRequested
          Just posts_ -> Available posts_
      posts = apiInit { endpoint = "/posts"
                      , method   = HttpGet
                      , decoder  = Json.field "posts" (Json.list dataPostDecoder) }
                |> andThen (setResource resource)
   in map HomePageModel
        (posts |> mapCmd HomePageApiMsg)

homePageUpdate : { onPostsLoaded : List DataPost -> a } -> HomePageMsg -> HomePageModel -> Update HomePageModel HomePageMsg a
homePageUpdate { onPostsLoaded } msg model =
  case msg of
    HomePageApiMsg apiMsg ->
      model.posts
        |> apiUpdate { apiDefaultHandlers | onSuccess = \posts -> invokeHandler (onPostsLoaded posts) } apiMsg
        |> mapCmd HomePageApiMsg
        |> andWithEvents (insertAsPostsIn model)
    FetchPosts ->
      model
        |> homePageUpdate { onPostsLoaded = onPostsLoaded } (HomePageApiMsg (Request "" Nothing))

homePageSubscriptions : HomePageModel -> Sub HomePageMsg
homePageSubscriptions model = Sub.none

listItem : DataPost -> Html HomePageMsg
listItem post =
  let url = "/posts/" ++ String.fromInt post.id
   in div []
        [ h1 [] [ text post.title ]
        , p [] [ text post.body ]
        , p [] [ a [ href url ] [ text "Show" ]
               , text " | "
               , a [ href (url ++ "/comment") ] [ text "Comment" ] ] ]

homePageView : HomePageModel -> Html HomePageMsg
homePageView { posts } =
  case posts.resource of
    NotRequested ->
      div [] [ text "Not requested" 
             , button [ onClick FetchPosts ] [ text "Fetch" ] ]
    Requested ->
      div [] [ text "Requested..." ]
    Error error ->
      div [] [ text "error" ]
    Available list ->
      div [] (List.map listItem list)

--

type PostsCreateMsg
  = PostsCreateApiMsg (ApiMsg DataPost)
  | PostsCreateFormMsg (FormMsg PostsCreateForm)

type alias PostsCreateModel =
  { post : ApiModel DataPost
  , form : FormModel PostsCreateForm }

insertAsPostIn : PostsCreateModel -> ApiModel DataPost -> Update PostsCreateModel PostsCreateMsg a
insertAsPostIn model post = save { model | post = post }

insertAsFormIn : PostsCreateModel -> FormModel PostsCreateForm -> Update PostsCreateModel PostsCreateMsg a
insertAsFormIn model form = save { model | form = form }

postsCreateInit : Update PostsCreateModel PostsCreateMsg a
postsCreateInit =
  let api = apiInit { endpoint = "/posts"
                    , method   = HttpPost
                    , decoder  = Json.field "post" dataPostDecoder }
      form = formInit postsCreateFormFields { title = "", body = "" }
   in map2 PostsCreateModel
        (api  |> mapCmd PostsCreateApiMsg)
        (form |> mapCmd PostsCreateFormMsg)

postsCreateHandleSubmit : { onPostAdded : DataPost -> a -> Update a c e } -> PostsCreateForm -> PostsCreateModel -> Update PostsCreateModel PostsCreateMsg (a -> Update a c e)
postsCreateHandleSubmit events form model =
  let json = postsCreateFormToJson form
   in postsCreateUpdate events (PostsCreateApiMsg (apiJsonRequest "" json)) model

postsCreateUpdate : { onPostAdded : DataPost -> a -> Update a c e } -> PostsCreateMsg -> PostsCreateModel -> Update PostsCreateModel PostsCreateMsg (a -> Update a c e)
postsCreateUpdate { onPostAdded } msg model =
  case msg of
    PostsCreateApiMsg apiMsg ->
      model.post
        |> apiUpdate { apiDefaultHandlers | onSuccess = invokeHandler << onPostAdded } apiMsg
        |> mapCmd PostsCreateApiMsg
        |> andWithEvents (insertAsPostIn model)
    PostsCreateFormMsg formMsg ->
      model.form
        |> formUpdate { onSubmit = postsCreateHandleSubmit { onPostAdded = onPostAdded } } formMsg
        |> mapCmd PostsCreateFormMsg
        |> andWithEvents (insertAsFormIn model)

postsCreateSubscriptions : PostsCreateModel -> Sub PostsCreateMsg
postsCreateSubscriptions model = Sub.none

postsCreateView : PostsCreateModel -> Html PostsCreateMsg
postsCreateView { form } =
  Html.map PostsCreateFormMsg (formView form)

--

type AuthLoginMsg
  = AuthLoginApiMsg (ApiMsg DataUser)
  | AuthLoginFormMsg (FormMsg AuthLoginForm)

type alias AuthLoginModel =
  { user : ApiModel DataUser
  , form : FormModel AuthLoginForm }

authLoginInsertAsUserIn : AuthLoginModel -> ApiModel DataUser -> Update AuthLoginModel AuthLoginMsg a
authLoginInsertAsUserIn model user = save { model | user = user }

authLoginInsertAsFormIn : AuthLoginModel -> FormModel AuthLoginForm -> Update AuthLoginModel AuthLoginMsg a
authLoginInsertAsFormIn model form = save { model | form = form }

authLoginInit : Update AuthLoginModel AuthLoginMsg e
authLoginInit =
  let api = apiInit { endpoint = "/auth/login"
                    , method   = HttpPost
                    , decoder  = Json.field "user" dataUserDecoder }
      form = formInit authLoginFormFields { login = "", password = "" }
   in map2 AuthLoginModel
        (api  |> mapCmd AuthLoginApiMsg)
        (form |> mapCmd AuthLoginFormMsg)

authLoginHandleSubmit : { onResponse : Maybe DataUser -> a -> Update a c e } -> AuthLoginForm -> AuthLoginModel -> Update AuthLoginModel AuthLoginMsg (a -> Update a c e)
authLoginHandleSubmit events form model =
  let json = authLoginFormToJson form
   in authLoginUpdate events (AuthLoginApiMsg (apiJsonRequest "" json)) model

authLoginUpdate : { onResponse : Maybe DataUser -> a -> Update a c e } -> AuthLoginMsg -> AuthLoginModel -> Update AuthLoginModel AuthLoginMsg (a -> Update a c e)
authLoginUpdate { onResponse } msg model =
  case msg of
    AuthLoginApiMsg apiMsg ->
      model.user
        |> apiUpdate { onSuccess = \user -> invokeHandler (onResponse (Just user))
                     , onError   = always (invokeHandler (onResponse Nothing)) } apiMsg
        |> mapCmd AuthLoginApiMsg
        |> andWithEvents (authLoginInsertAsUserIn model)
    AuthLoginFormMsg formMsg ->
      model.form
        |> formUpdate { onSubmit = authLoginHandleSubmit { onResponse = onResponse } } formMsg
        |> mapCmd AuthLoginFormMsg
        |> andWithEvents (authLoginInsertAsFormIn model)

authLoginSubscriptions : AuthLoginModel -> Sub AuthLoginMsg
authLoginSubscriptions model = Sub.none

authLoginView : AuthLoginModel -> Html AuthLoginMsg
authLoginView { form } =
  Html.map AuthLoginFormMsg (formView form)

--

type AuthRegisterMsg
  = AuthRegisterApiMsg (ApiMsg { status : String })
  | AuthRegisterFormMsg (FormMsg AuthRegisterForm)

type alias AuthRegisterModel =
  { response : ApiModel { status : String }
  , form     : FormModel AuthRegisterForm }

authRegisterInsertAsResponseIn : AuthRegisterModel -> ApiModel { status : String } -> Update AuthRegisterModel AuthRegisterMsg a
authRegisterInsertAsResponseIn model response = save { model | response = response }

authRegisterInsertAsFormIn : AuthRegisterModel -> FormModel AuthRegisterForm -> Update AuthRegisterModel AuthRegisterMsg a
authRegisterInsertAsFormIn model form = save { model | form = form }

authRegisterInit : Update AuthRegisterModel AuthRegisterMsg e
authRegisterInit =
  let decoder = Json.field "status" Json.string |> Json.map (\status -> { status = status })
      api = apiInit { endpoint = "/auth/register"
                    , method   = HttpPost
                    , decoder  = decoder }
      form = formInit authRegisterFormFields { login = "", password = "" }
   in map2 AuthRegisterModel
        (api  |> mapCmd AuthRegisterApiMsg)
        (form |> mapCmd AuthRegisterFormMsg)

authRegisterUpdate : AuthRegisterMsg -> AuthRegisterModel -> Update AuthRegisterModel AuthRegisterMsg e
authRegisterUpdate msg model =
  case msg of
    AuthRegisterApiMsg apiMsg ->
      model.response
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd AuthRegisterApiMsg
        |> andWithEvents (authRegisterInsertAsResponseIn model)
    AuthRegisterFormMsg formMsg ->
      model.form
        |> formUpdate { onSubmit = always save } formMsg
        |> mapCmd AuthRegisterFormMsg
        |> andWithEvents (authRegisterInsertAsFormIn model)

authRegisterSubscriptions : AuthRegisterModel -> Sub AuthRegisterMsg
authRegisterSubscriptions model = Sub.none

authRegisterView : AuthRegisterModel -> Html AuthRegisterMsg
authRegisterView { form } =
  Html.map AuthRegisterFormMsg (formView form)

--

type RouterMsg
  = UrlChange Url
  | UrlRequest UrlRequest
  | Redirect String

type alias RouterModel =
  { route : Maybe Route
  , key   : Navigation.Key }

setRoute : Maybe Route -> RouterModel -> Update RouterModel RouterMsg a
setRoute route model = save { model | route = route }

routerInit : Flags -> Url -> Navigation.Key -> Update RouterModel RouterMsg a
routerInit flags url key = save { route = Nothing, key = key }

routerUpdate : { onRouteChange : Maybe Route -> a -> Update a c e } -> RouterMsg -> RouterModel -> Update RouterModel RouterMsg (a -> Update a c e)
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

routerSubscriptions : RouterModel -> Sub RouterMsg
routerSubscriptions model = Sub.none

--

type UiMsg
  = NoUiMsg

type alias UiModel =
  {}

uiInit : Flags -> Url -> Navigation.Key -> Update UiModel UiMsg a
uiInit flags url key = save {}

uiUpdate : UiMsg -> UiModel -> Update UiModel UiMsg a
uiUpdate msg model = save model

uiSubscriptions : UiModel -> Sub UiMsg
uiSubscriptions model = Sub.none

--

type CacheData
  = CachePosts (List DataPost)

type CacheMsg
  = InsertData CacheData
  | ClearPosts

type alias CacheModel = 
  { posts : Maybe (List DataPost) }

cacheInit : Update CacheModel CacheMsg a
cacheInit = save { posts = Nothing }

cacheUpdate : CacheMsg -> CacheModel -> Update CacheModel CacheMsg a
cacheUpdate msg model =
  case msg of
    InsertData (CachePosts posts) ->
      save { model | posts = Just posts }
    ClearPosts ->
      save { model | posts = Nothing }

--

type Page
  = HomePage HomePageModel
  | NewPostPage PostsCreateModel
  | ShowPostPage PostsShowModel
  | CommentPostPage PostsCommentCreateModel
  | LoginPage AuthLoginModel
  | RegisterPage AuthRegisterModel
  | NotFoundPage

type PageMsg
  = PostsCreateMsg PostsCreateMsg
  | HomePageMsg HomePageMsg
  | PostsShowMsg PostsShowMsg
  | PostsCommentCreateMsg PostsCommentCreateMsg
  | AuthLoginMsg AuthLoginMsg
  | AuthRegisterMsg AuthRegisterMsg

--

type Msg
  = RouterMsg RouterMsg
  | UiMsg UiMsg
  | PageMsg PageMsg
  | CacheMsg CacheMsg

type alias Flags = ()

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : Page
  , user   : Maybe DataUser
  , cache  : CacheModel }

insertAsRouterIn : Model -> RouterModel -> Update Model Msg a
insertAsRouterIn model router = save { model | router = router }

insertAsUiIn : Model -> UiModel -> Update Model Msg a
insertAsUiIn model ui = save { model | ui = ui }

insertAsPageIn : Model -> Page -> Update Model Msg a
insertAsPageIn model page = save { model | page = page }

insertAsCacheIn : Model -> CacheModel -> Update Model Msg a
insertAsCacheIn model cache = save { model | cache = cache }

setUser : Maybe DataUser -> Model -> Update Model Msg a
setUser user model = save { model | user = user }

init : Flags -> Url -> Navigation.Key -> Update Model Msg (a -> Update a c e)
init flags url key =
  let router = routerInit flags url key
      ui     = uiInit flags url key
      page   = map NewPostPage postsCreateInit
   in map5 Model
        (router |> mapCmd RouterMsg)
        (ui     |> mapCmd UiMsg)
        (page   |> mapCmd (PageMsg << PostsCreateMsg))
        (save Nothing)
        (save { posts = Nothing })
          |> andThen (update (RouterMsg (UrlChange url)))
--          |> andThen (update ((PageMsg (HomePageMsg Refresh))))

handleRouteChange : CacheModel -> Maybe Route -> Model -> Update Model Msg (a -> Update a c e)
handleRouteChange cache route model =
  case route of
    Just Home ->
      homePageInit cache.posts
        |> mapCmd (PageMsg << HomePageMsg)
        |> andThen (\pageModel -> insertAsPageIn model (HomePage pageModel))
    Just NewPost ->
      postsCreateInit
        |> mapCmd (PageMsg << PostsCreateMsg)
        |> andThen (\pageModel -> insertAsPageIn model (NewPostPage pageModel))
    Just (Post id) ->
      postsShowInit id
        |> mapCmd (PageMsg << PostsShowMsg)
        |> andThen (\pageModel -> insertAsPageIn model (ShowPostPage pageModel))
    Just (CommentPost postId) ->
      postsCommentCreateInit postId
        |> mapCmd (PageMsg << PostsCommentCreateMsg)
        |> andThen (\pageModel -> insertAsPageIn model (CommentPostPage pageModel))
    Just Login ->
      authLoginInit
        |> mapCmd (PageMsg << AuthLoginMsg)
        |> andThen (\pageModel -> insertAsPageIn model (LoginPage pageModel))
    Just Register ->
      authRegisterInit
        |> mapCmd (PageMsg << AuthRegisterMsg)
        |> andThen (\pageModel -> insertAsPageIn model (RegisterPage pageModel))
    _ ->
      save { model | page = NotFoundPage }

updatePage : { redirect : String -> a -> Update a c e, onAuthUserChange : Maybe DataUser -> a -> Update a c e, updateCache : CacheData -> a -> Update a c e } -> PageMsg -> Page -> Update Page PageMsg (a -> Update a c e)
updatePage { redirect, onAuthUserChange, updateCache } msg page =
  case ( msg, page ) of
    ( PostsCreateMsg postsCreateMsg, NewPostPage postsCreateModel ) ->
      postsCreateModel
        |> postsCreateUpdate { onPostAdded = always (invokeHandler (redirect "/")) } postsCreateMsg
        |> mapCmd PostsCreateMsg
        |> map NewPostPage
        |> foldEvents
    ( HomePageMsg homePageMsg, HomePage homePageModel ) ->
      homePageModel
        |> homePageUpdate { onPostsLoaded = \posts -> invokeHandler (updateCache (CachePosts posts)) } homePageMsg
        |> mapCmd HomePageMsg
        |> map HomePage
        |> foldEvents
    ( PostsShowMsg postsShowMsg, ShowPostPage postsShowModel ) ->
      postsShowModel
        |> postsShowUpdate postsShowMsg
        |> mapCmd PostsShowMsg
        |> map ShowPostPage
        |> foldEvents
    ( PostsCommentCreateMsg postsCommentCreateMsg, CommentPostPage postsCommentCreateModel ) ->
      postsCommentCreateModel
        |> postsCommentCreateUpdate postsCommentCreateMsg
        |> mapCmd PostsCommentCreateMsg
        |> map CommentPostPage
        |> foldEvents
    ( AuthLoginMsg authLoginMsg, LoginPage authLoginModel ) ->
      authLoginModel
        |> authLoginUpdate { onResponse = invokeHandler << onAuthUserChange } authLoginMsg
        |> mapCmd AuthLoginMsg
        |> map LoginPage
        |> foldEvents
    ( AuthRegisterMsg authRegisterMsg, RegisterPage authRegisterModel ) ->
      authRegisterModel
        |> authRegisterUpdate authRegisterMsg
        |> mapCmd AuthRegisterMsg
        |> map RegisterPage
        |> foldEvents
    _ ->
      save page

handleRedirect : String -> Model -> Update Model Msg (a -> Update a c e)
handleRedirect url = update (RouterMsg (Redirect url))

update : Msg -> Model -> Update Model Msg (a -> Update a c e)
update msg model =
  case msg of
    RouterMsg routerMsg ->
      model.router
        |> routerUpdate { onRouteChange = handleRouteChange model.cache } routerMsg
        |> mapCmd RouterMsg
        |> andWithEvents (insertAsRouterIn model)
    UiMsg uiMsg ->
      model.ui
        |> uiUpdate uiMsg
        |> mapCmd UiMsg
        |> andWithEvents (insertAsUiIn model)
    CacheMsg cacheMsg ->
      model.cache
        |> cacheUpdate cacheMsg
        |> mapCmd CacheMsg
        |> andWithEvents (insertAsCacheIn model)
    PageMsg pageMsg ->
      updatePage { redirect = handleRedirect, onAuthUserChange = setUser
                 , updateCache = \data -> update (CacheMsg (InsertData data)) } pageMsg model.page
        |> mapCmd PageMsg
        |> andWithEvents (insertAsPageIn model)

pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
  case page of
    HomePage homePageModel ->
      Sub.map (PageMsg << HomePageMsg) (homePageSubscriptions homePageModel)
    NewPostPage postsCreateModel ->
      Sub.map (PageMsg << PostsCreateMsg) (postsCreateSubscriptions postsCreateModel)
    ShowPostPage postsShowModel ->
      Sub.map (PageMsg << PostsShowMsg) (postsShowSubscriptions postsShowModel)
    CommentPostPage postsCommentCreateModel ->
      Sub.map (PageMsg << PostsCommentCreateMsg) (postsCommentCreateSubscriptions postsCommentCreateModel)
    LoginPage authLoginModel ->
      Sub.map (PageMsg << AuthLoginMsg) (authLoginSubscriptions authLoginModel)
    RegisterPage authRegisterModel ->
      Sub.map (PageMsg << AuthRegisterMsg) (authRegisterSubscriptions authRegisterModel)
    NotFoundPage ->
      Sub.none

subscriptions : Model -> Sub Msg
subscriptions { page, router, ui } =
  Sub.batch
    ( pageSubscriptions page :: [ Sub.map RouterMsg (routerSubscriptions router)
                                , Sub.map UiMsg (uiSubscriptions ui) ] )

pageView : Page -> Html Msg
pageView page =
  case page of
    NewPostPage postsCreateModel ->
      Html.map (PageMsg << PostsCreateMsg) (postsCreateView postsCreateModel)
    ShowPostPage postsShowModel ->
      Html.map (PageMsg << PostsShowMsg) (postsShowView postsShowModel)
    CommentPostPage postsCommentCreateModel ->
      Html.map (PageMsg << PostsCommentCreateMsg) (postsCommentCreateView postsCommentCreateModel)
    HomePage homePageModel ->
      Html.map (PageMsg << HomePageMsg) (homePageView homePageModel)
    LoginPage authLoginModel ->
      Html.map (PageMsg << AuthLoginMsg) (authLoginView authLoginModel)
    RegisterPage authRegisterModel ->
      Html.map (PageMsg << AuthRegisterMsg) (authRegisterView authRegisterModel)
    NotFoundPage ->
      div [] [ text "not found" ]

view : Model -> Document Msg
view model =
  { title = ""
  , body =
    [ div []
      [ text "Hello"
      , ul []
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/posts/new" ] [ text "New post" ] ]
        , li [] [ a [ href "/login" ] [ text "Login" ] ]
        , li [] [ a [ href "/register" ] [ text "Register" ] ]
        , pageView model.page ]
      , text (Debug.toString model)
      ]
    ]
  }

--

main : Program Flags Model Msg
main =
  Browser.application
    { init          = \flags url key -> let ( a, b, _ ) = init flags url key in ( a, b )
    , update        = \msg model -> let ( a, b, _ ) = update msg model in ( a, b )
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = RouterMsg << UrlChange
    , onUrlRequest  = RouterMsg << UrlRequest }
