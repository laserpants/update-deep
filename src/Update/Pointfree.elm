module Update.Pointfree exposing (..)

type alias Update a msg e = (a -> ( a, Cmd msg, List e ))

copy : Update a msg e
copy model = ( model, Cmd.none, [] )

map : (msga -> msgb) -> Update a msga e -> Update a msgb e
map f u a =
  let ( model, cmd, events ) = u a
   in ( model, Cmd.map f cmd, events )

runCmd : Cmd msg -> Update a msg e
runCmd cmd model = ( model, cmd, [] )

andThen : Update a msg e -> Update a msg e -> Update a msg e
andThen u t a =
  let ( b, c, e ) = u a
      ( q, d, f ) = t b
   in ( q, Cmd.batch [ c, d ], e ++ f )

andRunCmd : Cmd msg -> Update a msg e -> Update a msg e
andRunCmd = andThen << runCmd

postEvent : e -> Update a msg e
postEvent event model = ( model, Cmd.none, [ event ] )

andPostEvent : e -> Update a msg e -> Update a msg e
andPostEvent = andThen << postEvent

within : { get : (b -> c), set : (c -> Update b a e) } -> Update c a e -> Update b a e
within { get, set } u a =
  let ( b, c, e ) = u (get a)
      ( q, d, f ) = set b a
   in ( q, Cmd.batch [ c, d ], e ++ f )

withinMap : { get : (b -> c), set : (c -> Update b a e) } -> (a -> d) -> Update c a e -> Update b d e
withinMap access msg = within access >> map msg

processEvents : (e -> Update b a e) -> Update b a e -> Update b a e
processEvents f u a =
  let ( model, cmd, events ) = u a
   in List.foldr (andThen << f) (\_ -> ( model, cmd, [] )) events a

runUpdate : Update a msg e -> a -> ( a, Cmd msg )
runUpdate u a = let ( model, cmds, _ ) = u a in ( model, cmds )

type alias Init m msg = { model : m, cmd : Cmd msg }

initial : m -> Init m msg
initial model = { model = model, cmd = Cmd.none }

initCmd : (msga -> msgb) -> { a | cmd : Cmd msga } -> Init m msgb -> Init m msgb
initCmd f init { model, cmd } =
  { model = model
  , cmd   = Cmd.batch [ cmd, Cmd.map f init.cmd ] }

runInit : ({ flags : flags, key : key, url : url } -> Init a msg) -> flags -> url -> key -> ( a, Cmd msg )
runInit init flags url key =
  let initd = init { flags = flags, url = url, key = key }
   in ( initd.model, initd.cmd )
