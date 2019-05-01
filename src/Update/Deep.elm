module Update.Deep exposing (..)

{-| A type alias wrapper for Elm's `( model, Cmd msg )` tuple that introduces a
third component to allow for callbacks to be passed down through the update hierarchy.
-}
type alias Update a c e = ( a, Cmd c, List e )

{-| This function lifts a value into the `Update` context, just like `return` in Haskell.
-}
save : a -> Update a c e
save state = ( state, Cmd.none, [] )

{-| Apply a function to the state part of the value.
-}
map : (a -> b) -> Update a c e -> Update b c e
map f ( state, cmd, handlers ) = ( f state, cmd, handlers )

{-| Removes one level of monadic structure. In particular, `andThen f = join << map f`
-}
join : Update (Update a c e) c e -> Update a c e
join ( ( state, c, e ), d, f ) = ( state, Cmd.batch [ c, d ], e ++ f )

{-|
Apply a function to the command part of the value. This is typically used to lift a
value returned from a lower-level update into the caller context.
-}
mapCmd : (c -> d) -> Update a c e -> Update a d e
mapCmd f ( state, cmd, handlers )  = ( state, Cmd.map f cmd, handlers )

{-| Add a command to the `Update` pipeline. For example;

    update msg state =
      case msg of
        SomeMsg someMsg ->
          state
            |> runCmd someCmd
            |> andThen (runCmd someOtherCmd)
-}
runCmd : Cmd c -> a -> Update a c e
runCmd cmd state = ( state, cmd, [] )

{-| Sequential composition of updates. This function is especially useful in combination
with the forward pipe operator (|>), for writing code in the style of pipelines. It is
just like the bind operator in Haskell, but with the arguments interchanged.
-}
andThen : (a -> Update b c e) -> Update a c e -> Update b c e
andThen f = join << map f

{-| Right-to-left (Kleisli) composition of two functions that return `Update` values,
passing the state part of the first return value to the second function.
-}
kleisli : (b -> Update d c e) -> (a -> Update b c e) -> a -> Update d c e
kleisli f g = andThen f << g

{-| Shortcut for `andThen << runCmd`
-}
andRunCmd : Cmd c -> Update a c e -> Update a c e
andRunCmd = andThen << runCmd

{-| Add a partially applied event handler to the list of monadic functions that
will be applied (sequentially) to the result of the nested update call.
-}
invoke : e -> a -> Update a c e
invoke handler state = ( state, Cmd.none, [ handler ] )

{-| Shortcut for `andThen << invoke`
-}
andInvoke : e -> Update a c e -> Update a c e
andInvoke = andThen << invoke

{-| Collapses the list of monadic functions (events) produced by a nested update
call, merging them into the current context.
-}
runEvents : Update a c (a -> Update a c e) -> Update a c e
runEvents ( a, c, list ) = List.foldr andThen ( a, c, [] ) list

{-| Shortcut for `\f -> runEvents << andThen f`
-}
runEventsAnd : (b -> Update a c (a -> Update a c e)) -> Update b c (a -> Update a c e) -> Update a c e
runEventsAnd f = runEvents << andThen f

runUpdate : (c -> a -> Update a c e) -> c -> a -> ( a, Cmd c )
runUpdate f msg state = let ( a, c, _ ) = f msg state in ( a, c )

{-| A wrapper for `( model, Cmd msg )` to make it more convenient to intialize
nested state. Typically used in the following way:

    -- Main.elm:

    type Msg
      = UsersMsg Users.Msg
      | BotsMsg Bots.Msg
      | GoatsMsg Goats.Msg

    type alias State = { ... }

    init : Flags -> Init State Msg
    init flags =
      let users = Users.init
          bots  = Bots.init
          goats = Goats.init
       in { users = users.state
          , bots  = bots.state
          , goats = goats.state }
            |> initial
            |> initCmd UsersMsg users
            |> initCmd BotsMsg bots
            |> initCmd GoatsMsg goats

    -- Users.elm:

    type Msg
      = GiveACookieTo User
      | ...

    type alias State = { ... }

    init : Init State Msg
    init = initial { ... }   -- Similar setup to init in Main.elm
-}
type alias Init a c = { state : a, cmd : Cmd c }

{-| Lifts a value into the `Init` context. (Similar to `save`.)
-}
initial : a -> Init a c
initial state = { state = state, cmd = Cmd.none }

{-| Apply commands returned by lower-level init calls in the caller context.
-}
initCmd : (c -> d) -> Init a c -> Init b d -> Init b d
initCmd msg a b = { state = b.state, cmd = Cmd.batch [ Cmd.map msg a.cmd, b.cmd ] }

applicationInit : (flags -> url -> key -> Init a c) -> flags -> url -> key -> ( a, Cmd c )
applicationInit init flags url key =
  let { state, cmd } = init flags url key in ( state, cmd )

documentInit : (flags -> Init a c) -> flags -> ( a, Cmd c )
documentInit init flags =
  let { state, cmd } = init flags in ( state, cmd )
