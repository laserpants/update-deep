module FormState exposing (..)

import Form exposing (Form)
import Form.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Update.Deep exposing (..)

type Msg a
  = FormChanged (Form.View.Model a)
  | Submit a
  | Reset

type alias FormState a =
  { model   : Form.View.Model a 
  , form    : Form a (Msg a)
  , initial : a }

type alias Fields a = Form a (Msg a)

init : Form a (Msg a) -> a -> Init (FormState a) (Msg a)
init form values =
  { model   = Form.View.idle values
  , form    = form
  , initial = values } |> initial

defaultHandlers : { onSubmit : b -> a -> Update a c e }
defaultHandlers = { onSubmit = always save }

update : { t | onSubmit : b -> a -> Update a c e } -> Msg b -> FormState b -> Update (FormState b) (Msg b) (a -> Update a c e)
update { onSubmit } msg state =
  case msg of
    Reset ->
      save { state | model = Form.View.idle state.initial }
    FormChanged formViewModel ->
      save { state | model = formViewModel }
    Submit values ->
      let { model } = state
       in save { state | model = { model | state = Form.View.Loading } }
        |> andInvoke (onSubmit values)

view : FormState a -> Html (Msg a)
view { form, model } =
  Form.View.asHtml
    { onChange   = FormChanged
    , action     = "Submit"
    , loading    = "Submit"
    , validation = Form.View.ValidateOnSubmit
    } form model
