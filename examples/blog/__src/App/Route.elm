module App.Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, parse, map, top, s, int, oneOf, (</>))

type Route
  = Home
  | About
  | Login
  | Register
  | ShowPost Int
  | NewPost
  | NewComment Int

parser : Parser (Route -> a) a
parser =
  oneOf
    [ map Home       top
    , map About      (s "about")
    , map Login      (s "login")
    , map Register   (s "register")
    , map ShowPost   (s "posts" </> int)
    , map NewPost    (s "posts" </> s "new")
    , map NewComment (s "posts" </> int </> s "comments" </> s "new") ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser
