module Update.Eventful exposing (..)

type alias Update a msg e = ( a, Cmd msg, List e )

m : Update a c e -> a
m ( state, _, _ ) = state

copy : a -> Update a c e
copy state = ( state, Cmd.none, [] )

map : (a -> b) -> Update a c e -> Update b c e
map f ( state, cmd, events ) = ( f state, cmd, events )

join : Update (Update a c e) c e -> Update a c e
join ( ( state, c, e ), d, f ) = ( state, Cmd.batch [ c, d ], e ++ f )

mapCmd : (c -> d) -> Update a c e -> Update a d e
mapCmd f ( state, cmd, events )  = ( state, Cmd.map f cmd, events )

runCmd : Cmd c -> a -> Update a c e
runCmd cmd state = ( state, cmd, [] )

andThen : (a -> Update b c e) -> Update a c e -> Update b c e
andThen f = join << map f

kleisli : (b -> Update d c e) -> (a -> Update b c e) -> a -> Update d c e
kleisli f g = andThen f << g

andRunCmd : Cmd c -> Update a c e -> Update a c e
andRunCmd = andThen << runCmd

postEvent : e -> a -> Update a c e
postEvent event state = ( state, Cmd.none, [ event ] )

andPostEvent : e -> Update a c e -> Update a c e
andPostEvent = andThen << postEvent

processEvents : (e -> a -> Update a c e) -> Update a c e -> Update a c e
processEvents process ( state, cmd, events ) =
  List.foldr (andThen << process) ( state, cmd, [] ) events

runUpdate : (c -> a -> Update a c e) -> c -> a -> ( a, Cmd c )
runUpdate f msg state = let ( a, c, _ ) = f msg state in ( a, c )

applicationInit : (flags -> url -> key -> Update a msg e) -> flags -> url -> key -> ( a, Cmd msg )
applicationInit init flags url key =
  let ( state, cmd, _ ) = init flags url key in ( state, cmd )

documentInit : (flags -> Update a c e) -> flags -> ( a, Cmd c )
documentInit init flags =
  let ( state, cmd, _ ) = init flags in ( state, cmd )
