module Update.Deep exposing (..)

type alias Update a c e = ( a, Cmd c, List e )

save : a -> Update a c e
save state = ( state, Cmd.none, [] )

map : (a -> b) -> Update a c e -> Update b c e
map f ( state, cmd, handlers ) = ( f state, cmd, handlers )

join : Update (Update a c e) c e -> Update a c e
join ( ( state, c, e ), d, f ) = ( state, Cmd.batch [ c, d ], e ++ f )

mapCmd : (c -> d) -> Update a c e -> Update a d e
mapCmd f ( state, cmd, handlers )  = ( state, Cmd.map f cmd, handlers )

runCmd : Cmd c -> a -> Update a c e
runCmd cmd state = ( state, cmd, [] )

andThen : (a -> Update b c e) -> Update a c e -> Update b c e
andThen f = join << map f

kleisli : (b -> Update d c e) -> (a -> Update b c e) -> a -> Update d c e
kleisli f g = andThen f << g

andRunCmd : Cmd c -> Update a c e -> Update a c e
andRunCmd = andThen << runCmd

invoke : e -> a -> Update a c e
invoke handler state = ( state, Cmd.none, [ handler ] )

andInvoke : e -> Update a c e -> Update a c e
andInvoke = andThen << invoke

runEvents : Update a c (a -> Update a c e) -> Update a c e
runEvents ( a, c, list ) = List.foldr andThen ( a, c, [] ) list

runEventsAnd : (b -> Update a c (a -> Update a c e)) -> Update b c (a -> Update a c e) -> Update a c e
runEventsAnd f = runEvents << andThen f

runUpdate : (c -> a -> Update a c e) -> c -> a -> ( a, Cmd c )
runUpdate f msg state = let ( a, c, _ ) = f msg state in ( a, c )

type alias Init a c = { state : a, cmd : Cmd c }

initial : a -> Init a c
initial state = { state = state, cmd = Cmd.none }

initCmd : (c -> d) -> Init a c -> Init b d -> Init b d
initCmd msg a b = { state = b.state, cmd = Cmd.batch [ Cmd.map msg a.cmd, b.cmd ] }

applicationInit : (flags -> url -> key -> Init a c) -> flags -> url -> key -> ( a, Cmd c )
applicationInit init flags url key =
  let { state, cmd } = init flags url key in ( state, cmd )

documentInit : (flags -> Init a c) -> flags -> ( a, Cmd c )
documentInit init flags =
  let { state, cmd } = init flags in ( state, cmd )
