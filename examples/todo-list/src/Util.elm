module Util exposing (..)

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

const : x -> y -> x
const a _ = a

curry : (( a, b ) -> c) -> a -> b -> c
curry f a b = f ( a, b )

uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) = f a b
