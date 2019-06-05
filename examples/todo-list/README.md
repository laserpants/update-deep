# To Do-List App Example

#### Run this example in the browser [here](https://laserpants.github.io/update-deep/examples/todo-list).

The application consists of the following modules:

```
               ┌────────────┐               |
          ┌────│    Main    │────┐          |
          │    └────────────┘    │          |   ┌───────────────────┐
 ┌─────── ▼ ───────┐       ┌──── ▼ ────┐    |   │   Data.TodoItem   │
 │  Notifications  │       │   Todos   │    |   └───────────────────┘
 └─────────────────┘       └─────┬─────┘    |   ┌───────────────────┐         
                                 │          |   │ Data.Notification │         
                         ┌────── ▼ ─────┐   |   └───────────────────┘         
                         │  Todos.Form  │   |
                         └──────────────┘   |
```

The `Data.TodoItem` module defines the `TodoItem` type

```elm
type alias TodoItem = { text : String }
```

which holds a description of the anticipated task. `Data.Notification` is similar. It represents a “toast” notification shown on the screen.

Let's concentrate instead on the four modules on the left side of the diagram; `Main`, `Notifications`, `Todos`, and `Todos.Form`.
Each one of these specifies its own `Msg` and `State` type, as well as `update` and `init` functions. (Subscriptions are not used in this example.)

> Note that *state* is used here to refer to (what the Elm architecture calls) a *model*, and that these two terms are used more or less interchangeably in the following.

It is useful to imagine these as instances of the following blueprint:

```elm
module Template exposing (..)

type Msg
    = SomeMsg
    | SomeOtherMsg
    | -- etc.

type alias State =
    { 
        -- ...
    }

init : Update State msg a
init = 
    save {}

update : Msg -> State -> Update State msg a
update msg state =
    case msg of
        -- etc.

view = ...
```

The only thing that makes this different is the return types of `update` and `init`.
Here is the definition of the `Update` type alias:

```elm
type alias Update m c e =
    ( m, Cmd c, List e )
```

This is just the usual model-`Cmd` pair with an extra, third element.
As you may have guessed already, writing `save {}` in the above code, is the same as returning `( {}, Cmd.none, [] )`.
We typically manipulate these values by composing functions of the form `something -> State -> Update State msg a` in the familiar style using pipes:

```elm
save state
    |> andThen doSomething
    |> andThen doSomethingElse
```

```elm
state
    |> addCmd (Ports.clearSession ())
    |> andThen doSomething
```

How is this useful then? Well, messages move down in the update tree. To pass information in the opposite direction, this library introduces a simple, callback-based event handling mechanism. That is what the third element of the `Update` tuple is for.

In this example, there are three event handlers involved:

```
               ┌────────────┐
               │    Main    │
               └── ▲ ─ ▲ ───┘
                   │   │
                   │   │--- onTaskAdded
     onTaskDone ---│   │
                   │   │   ┌───────────┐
                   └───┴───│   Todos   │
                           └──── ▲ ────┘
                                 │
                                 │--- onSubmit
                                 │
                         ┌───────┴──────┐
                         │  Todos.Form  │
                         └──────────────┘
```

When a task is added or completed, `Main` gets a chance to update itself, in this case so that we can show a notification (toast).
Similarly, `Todos` is told when the form is submitted, so that it can add the new `TodoItem` to its list. Let's look at `update` in `Todos.Form`:

```elm
-- src/Todos/Form.elm (line 30)

update : { onSubmit : FormData -> a } -> Msg -> State -> Update State msg a
update { onSubmit } msg state =
    case msg of
        Submit ->
            state
                |> invokeHandler (onSubmit { text = state.text })
                |> andThen (setText "")
        
        Focus ->
            -- etc.
```

An `onSubmit` callback is 

Now,

```elm
-- src/Todos.elm (line 52)

    let
        handleSubmit data =
            let
                item =
                    { text = data.text }
            in
            addItem item
                >> andInvokeHandler (onTaskAdded item)

-- src/Todos.elm (line 71)

    in
    case msg of
        TodosFormMsg formMsg ->
            inForm (Form.update { onSubmit = handleSubmit } formMsg)

        -- etc.
```

`addItem` (lo and behold) adds the `TodoItem` to the list of tasks.

Finally, in `Main.elm`

```elm
-- src/Main.elm (line 59)

update : Msg -> State -> Update State Msg a
update msg =
    case msg of
        TodosMsg todosMsg ->
            inTodos (Todos.update { onTaskAdded = handleItemAdded, onTaskDone = handleTaskDone } todosMsg)

        -- etc.
```

```elm
-- src/Main.elm (line 47)

handleItemAdded : TodoItem -> State -> Update State Msg a
handleItemAdded _ =
    Notifications.addNotification "A new task was added to your list." NotificationsMsg
        |> inNotifications
```
