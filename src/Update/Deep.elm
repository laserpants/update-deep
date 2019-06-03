module Update.Deep exposing (In, Update, addCmd, andAddCmd, andInvokeHandler, andMap, andThen, andThenIf, ap, applicationInit, documentInit, foldEvents, foldEventsAndThen, invokeHandler, join, kleisli, map, map2, map3, map4, map5, map6, map7, mapCmd, runUpdate, save, inState)


type alias Update m c e =
    ( m, Cmd c, List e )


save : m -> Update m c e
save model =
    ( model, Cmd.none, [] )


addCmd : Cmd c -> m -> Update m c e
addCmd cmd state =
    ( state, cmd, [] )


mapCmd : (c -> d) -> Update m c e -> Update m d e
mapCmd f ( model, cmd, events ) =
    ( model, Cmd.map f cmd, events )


invokeHandler : e -> m -> Update m c e
invokeHandler handler state =
    ( state, Cmd.none, [ handler ] )


ap : Update (a -> b) c e -> Update a c e -> Update b c e
ap ( f, cmda, e ) ( model, cmdb, e2 ) =
    ( f model, Cmd.batch [ cmda, cmdb ], e ++ e2 )


andMap : Update a c e -> Update (a -> b) c e -> Update b c e
andMap a b =
    ap b a


map : (a -> b) -> Update a c e -> Update b c e
map f ( model, cmd, events ) =
    ( f model, cmd, events )


map2 : (a -> b -> p) -> Update a c e -> Update b c e -> Update p c e
map2 f =
    ap << map f


map3 : (a -> b -> p -> q) -> Update a c e -> Update b c e -> Update p c e -> Update q c e
map3 f x =
    ap << map2 f x


map4 : (a -> b -> p -> q -> r) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e
map4 f x y =
    ap << map3 f x y


map5 : (a -> b -> p -> q -> r -> s) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e
map5 f x y z =
    ap << map4 f x y z


map6 : (a -> b -> p -> q -> r -> s -> t) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e
map6 f x y z a =
    ap << map5 f x y z a


map7 : (a -> b -> p -> q -> r -> s -> t -> u) -> Update a c e -> Update b c e -> Update p c e -> Update q c e -> Update r c e -> Update s c e -> Update t c e -> Update u c e
map7 f x y z a b =
    ap << map6 f x y z a b


join : Update (Update a c e) c e -> Update a c e
join ( ( model, cmda, e ), cmdb, e2 ) =
    ( model, Cmd.batch [ cmda, cmdb ], e ++ e2 )


andThen : (b -> Update a c e) -> Update b c e -> Update a c e
andThen fun =
    join << map fun


andThenIf : (a -> Bool) -> (a -> Update a c e) -> Update a c e -> Update a c e
andThenIf pred fun upd =
    map pred upd
        |> andThen
            (\cond ->
                if cond then
                    join (map fun upd)

                else
                    upd
            )


kleisli : (b -> Update d c e) -> (a -> Update b c e) -> (a -> Update d c e)
kleisli f g =
    andThen f << g


andAddCmd : Cmd c -> Update a c e -> Update a c e
andAddCmd =
    andThen << addCmd


andInvokeHandler : e -> Update a c e -> Update a c e
andInvokeHandler =
    andThen << invokeHandler


foldEvents : Update a c (a -> Update a c e) -> Update a c e
foldEvents ( m, cmd, events ) =
    List.foldr andThen ( m, cmd, [] ) events


foldEventsAndThen : (m -> Update a c (a -> Update a c e)) -> Update m c (a -> Update a c e) -> Update a c e
foldEventsAndThen fun =
    foldEvents << andThen fun


type alias In state slice msg a =
    (slice -> Update slice msg (state -> Update state msg a)) -> state -> Update state msg a


inState : { get : b -> d, set : b -> m -> a } -> (d -> Update m c (a -> Update a c e)) -> b -> Update a c e
inState { get, set } fun state =
    get state |> fun |> foldEventsAndThen (set state >> save)


applicationInit : (d -> e -> f -> Update a b c) -> d -> e -> f -> ( a, Cmd b )
applicationInit f a b c =
    let
        ( model, cmd, _ ) =
            f a b c
    in
    ( model, cmd )


documentInit : (f -> Update a b c) -> f -> ( a, Cmd b )
documentInit f a =
    let
        ( model, cmd, _ ) =
            f a
    in
    ( model, cmd )


runUpdate : (d -> e -> Update a b c) -> d -> e -> ( a, Cmd b )
runUpdate f a b =
    let
        ( model, cmd, _ ) =
            f a b
    in
    ( model, cmd )
