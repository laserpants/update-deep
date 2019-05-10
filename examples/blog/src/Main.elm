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

consumeEvents : Update a c (a -> Update a c e) -> Update a c e
consumeEvents ( m, cmd, events ) = List.foldr andThen ( m, cmd, [] ) events

andWithEvents : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
andWithEvents setter = consumeEvents << andThen setter 

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

apiDefaultHandlers : { onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e }
apiDefaultHandlers = { onSuccess = always save, onError = always save }

apiUpdate : { onSuccess : b -> a -> Update a c e, onError : Http.Error -> a -> Update a c e } -> ApiMsg b -> ApiModel b -> Update (ApiModel b) (ApiMsg b) (a -> Update a c e)
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

formInit : Form a (FormMsg a) -> a -> Update (FormModel a) (FormMsg a) e
formInit form values =
  save
    { state   = Form.View.idle values
    , form    = form
    , initial = values }

formUpdate : { onSubmit : b -> a -> Update a c e } -> FormMsg b -> FormModel b -> Update (FormModel b) (FormMsg b) (a -> Update a c e)
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
--
--

type Route
  = Home
  | About
  | NewPost
  | Post Int
  | Login
  | Register

parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home     (Parser.top)
    , Parser.map About    (Parser.s "about")
    , Parser.map Login    (Parser.s "login")
    , Parser.map Register (Parser.s "register")
    , Parser.map NewPost  (Parser.s "posts" </> Parser.s "new")
    , Parser.map Post     (Parser.s "posts" </> Parser.int) ]

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

type PostsListMsg 
  = PostsListApiMsg (ApiMsg (List DataPost))
  | Refresh

type alias PostsListModel =
  { collection : ApiModel (List DataPost) }

insertAsCollectionIn : PostsListModel -> ApiModel (List DataPost) -> Update PostsListModel PostsListMsg a
insertAsCollectionIn model collection = save { model | collection = collection }

postsListInit : Update PostsListModel PostsListMsg a
postsListInit = 
  let collection = apiInit { endpoint = "/posts"
                           , method   = HttpGet
                           , decoder  = Json.field "posts" (Json.list dataPostDecoder) }
   in map PostsListModel
        (collection |> mapCmd PostsListApiMsg) 

postsListUpdate : PostsListMsg -> PostsListModel -> Update PostsListModel PostsListMsg a
postsListUpdate msg model = 
  case msg of
    PostsListApiMsg apiMsg ->
      model.collection
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd PostsListApiMsg
        |> andWithEvents (insertAsCollectionIn model)
    Refresh ->
      model
        |> postsListUpdate (PostsListApiMsg (Request "" Nothing))

postsListSubscriptions : PostsListModel -> Sub PostsListMsg
postsListSubscriptions model = Sub.none

listItem : DataPost -> Html PostsListMsg
listItem post =
  div [] 
    [ h1 [] [ text post.title ] 
    , p [] [ text post.body ] ]

postsListView : PostsListModel -> Html PostsListMsg
postsListView { collection } =
  case collection.resource of
    NotRequested ->
      div [] [ text "Not requested" ]
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

handleSubmit : { onPostAdded : DataPost -> a -> Update a c e } -> PostsCreateForm -> PostsCreateModel -> Update PostsCreateModel PostsCreateMsg (a -> Update a c e)
handleSubmit events form model = 
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
        |> formUpdate { onSubmit = handleSubmit { onPostAdded = onPostAdded } } formMsg
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

authLoginUpdate : AuthLoginMsg -> AuthLoginModel -> Update AuthLoginModel AuthLoginMsg e
authLoginUpdate msg model =
  case msg of
    AuthLoginApiMsg apiMsg ->
      model.user
        |> apiUpdate apiDefaultHandlers apiMsg
        |> mapCmd AuthLoginApiMsg
        |> andWithEvents (authLoginInsertAsUserIn model)
    AuthLoginFormMsg formMsg ->
      model.form
        |> formUpdate { onSubmit = always save } formMsg
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

type Page
  = HomePage PostsListModel
  | NewPostPage PostsCreateModel
  | LoginPage AuthLoginModel
  | RegisterPage AuthRegisterModel
  | NotFoundPage

type PageMsg
  = PostsCreateMsg PostsCreateMsg
  | PostsListMsg PostsListMsg
  | AuthLoginMsg AuthLoginMsg
  | AuthRegisterMsg AuthRegisterMsg

--

type Msg
  = RouterMsg RouterMsg
  | UiMsg UiMsg
  | PageMsg PageMsg

type alias Flags = ()

type alias Model =
  { router : RouterModel
  , ui     : UiModel
  , page   : Page }

insertAsRouterIn : Model -> RouterModel -> Update Model Msg a
insertAsRouterIn model router = save { model | router = router }

insertAsUiIn : Model -> UiModel -> Update Model Msg a
insertAsUiIn model ui = save { model | ui = ui }

insertAsPageIn : Model -> Page -> Update Model Msg a
insertAsPageIn model page = save { model | page = page }

init : Flags -> Url -> Navigation.Key -> Update Model Msg (a -> Update a c e)
init flags url key =
  let router = routerInit flags url key
      ui     = uiInit flags url key
      page   = map NewPostPage postsCreateInit
   in map3 Model
        (router |> mapCmd RouterMsg)
        (ui     |> mapCmd UiMsg)
        (page   |> mapCmd (PageMsg << PostsCreateMsg))
          |> andThen (update (RouterMsg (UrlChange url)))
          |> andThen (update ((PageMsg (PostsListMsg Refresh))))

handleRouteChange : Maybe Route -> Model -> Update Model Msg (a -> Update a c e)
handleRouteChange route model =
  case route of
    Just Home ->
      postsListInit
        |> mapCmd (PageMsg << PostsListMsg)
        |> andThen (\pageModel -> insertAsPageIn model (HomePage pageModel))
        |> andThen (update (PageMsg (PostsListMsg Refresh)))
    Just NewPost ->
      postsCreateInit
        |> mapCmd (PageMsg << PostsCreateMsg)
        |> andThen (\pageModel -> insertAsPageIn model (NewPostPage pageModel))
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

doRedirect : String -> Model -> Update Model Msg (a -> Update a c e)
doRedirect url = update (RouterMsg (Redirect url))

updatePage : { redirect : String -> a -> Update a c e } -> PageMsg -> Page -> Update Page PageMsg (a -> Update a c e)
updatePage { redirect } msg page =
  case ( msg, page ) of
    ( PostsCreateMsg postsCreateMsg, NewPostPage postsCreateModel ) ->
      postsCreateModel
        |> postsCreateUpdate { onPostAdded = always (invokeHandler (redirect "/")) } postsCreateMsg
        |> mapCmd PostsCreateMsg
        |> map NewPostPage
        |> consumeEvents
    ( PostsListMsg postsListMsg, HomePage postsListModel ) ->
      postsListModel
        |> postsListUpdate postsListMsg
        |> mapCmd PostsListMsg
        |> map HomePage
        |> consumeEvents
    ( AuthLoginMsg authLoginMsg, LoginPage authLoginModel ) ->
      authLoginModel
        |> authLoginUpdate authLoginMsg
        |> mapCmd AuthLoginMsg
        |> map LoginPage
        |> consumeEvents
    ( AuthRegisterMsg authRegisterMsg, RegisterPage authRegisterModel ) ->
      authRegisterModel
        |> authRegisterUpdate authRegisterMsg
        |> mapCmd AuthRegisterMsg
        |> map RegisterPage
        |> consumeEvents
    _ ->
      save page

update : Msg -> Model -> Update Model Msg (a -> Update a c e)
update msg model =
  case msg of
    RouterMsg routerMsg ->
      model.router
        |> routerUpdate { onRouteChange = handleRouteChange } routerMsg
        |> mapCmd RouterMsg 
        |> andWithEvents (insertAsRouterIn model)
    UiMsg uiMsg ->
      model.ui
        |> uiUpdate uiMsg
        |> mapCmd UiMsg 
        |> andWithEvents (insertAsUiIn model)
    PageMsg pageMsg ->
      updatePage { redirect = doRedirect } pageMsg model.page
        |> mapCmd PageMsg
        |> andWithEvents (insertAsPageIn model)

pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
  case page of
    HomePage postsListModel ->
      Sub.map (PageMsg << PostsListMsg) (postsListSubscriptions postsListModel)
    NewPostPage postsCreateModel ->
      Sub.map (PageMsg << PostsCreateMsg) (postsCreateSubscriptions postsCreateModel)
    LoginPage authLoginModel ->
      Sub.map (PageMsg << AuthLoginMsg) (authLoginSubscriptions authLoginModel)
    RegisterPage authRegisterModel ->
      Sub.map (PageMsg << AuthRegisterMsg) (authRegisterSubscriptions authRegisterModel)
    _ ->
      Sub.none

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    ( pageSubscriptions model.page :: [ Sub.map RouterMsg (routerSubscriptions model.router)
                                      , Sub.map UiMsg (uiSubscriptions model.ui) ] )

pageView : Page -> Html Msg
pageView page =
  case page of
    NewPostPage postsCreateModel ->
      Html.map (PageMsg << PostsCreateMsg) (postsCreateView postsCreateModel)
    HomePage postsListModel ->
      Html.map (PageMsg << PostsListMsg) (postsListView postsListModel)
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
