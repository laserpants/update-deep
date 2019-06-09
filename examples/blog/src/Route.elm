module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, parse, top, s, int, map, oneOf, (</>))

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
    [ map Home top
    , map Login (s "login")
    , map Logout (s "logout")
    , map Register (s "register")
    , map About (s "about")
    , map NewPost (s "posts" </> s "new")
    , map ShowPost (s "posts" </> int)
    ]

fromUrl : Url -> Maybe Route
fromUrl = parse parser
