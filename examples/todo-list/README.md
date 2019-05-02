# To Do-List Example

#### Run this example in the browser [here](https://laserpants.github.io/update-deep/examples/todo-list).

The application consists of the following modules:

```
               ┌────────────┐               |
          ┌────│    Main    │────┐          |   
          │    └────────────┘    │          |   ┌─────────────────┐
 ┌─────── ▼ ───────┐       ┌──── ▼ ────┐    |   │  Data.TodoItem  │
 │  Notifications  │       │   Todos   │    |   └─────────────────┘
 └─────────────────┘       └─────┬─────┘    |   ┌────────┐     
                                 │          |   │  Util  │
                         ┌────── ▼ ─────┐   |   └────────┘
                         │  Todos.Form  │   | 
                         └──────────────┘   |  
```

In `Data.TodoItem`, we just define the `TodoItem` type; and `Util` contains a few helper functions, like `flip` and `const`. 
In the following, we are mostly concerned with the four modules on the left side of the diagram; `Main`, `Notifications`, `Todos`, and `Todos.Form`. 
Each one of these specifies its own `Msg` and `State` type, as well as `update` and `init` functions. 
Most of this is implemented as usual, but the return types of `update` and `init` are a bit different, and `update` takes an extra `EventHandlers` argument:

```elm
update : EventHandlers t a c e  -> Msg -> State -> Update State Msg (a -> Update a c e)
```

```elm
init : Flags -> Init State Msg
```

> Note that *state* is used here to refer to (what the Elm architecture calls) a *model*, and that these two terms are used more or less interchangeably in the following.

As usual, messages move down in the update tree. To pass information in the opposite direction, this library introduces a simple, callback-based event handling mechanism. 
In this example, there are three event handlers involved:

```
               ┌────────────┐
               │    Main    │      
               └── ▲ ─ ▲ ───┘      
                   │   │                   
                   │   │--- onItemAdded
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

When a task is added or completed, `Main` gets a chance to update itself, so that we can show a notification.
Similarly, `Todos` is interested to know when the form is submitted, so that it can add the new `TodoItem` to the list. Let's look at `update` in `Todos.Form`:

```elm
-- src/Todos/Form.elm (line 24)

update : EventHandlers t a c e -> Msg -> State -> Update State Msg (a -> Update a c e)
update { onSubmit } msg state =
  case msg of
    OnSubmit ->
      state
        |> setText ""
        |> andInvoke (onSubmit { text = state.text })
    OnFocus ->
      -- etc.
```

in `Todos`, the `onSubmit` handler receives the `TodoItem` and wraps it in an `AddItem` message&hellip;

```elm
-- src/Todos.elm (line 40)

update : EventHandlers t a c e  -> Msg -> State -> Update State Msg (a -> Update a c e)
update events msg state =
  case msg of
    FormMsg formMsg ->
      state.form
        |> Form.update { onSubmit = \todo -> update events (AddItem todo) } formMsg
        -- etc.
```

&hellip; and then calls `update` again.

```
             ┌──────────┐
             │          │ 
          AddTodo       │ 
             │          │ 
       ┌──── ▼ ────┐    │
       │   Todos   │────┘
       └──── ▲ ────┘
             │--- onSubmit
     ┌───────┴──────┐
     │  Todos.Form  │
     └──────────────┘
```

The subsequent lines are mostly boilerplate to insert the updated `Todos.Form` state back into the `Todo` model again, and to map the `FormMsg` constructor over the command value (so that it becomes a `Cmd Todos.Msg`).
Calling the event handlers from within `Todos.Form` produces a list of monadic functions, which is what the third type parameter in `Update` is for. 
We then use `consumeEvents` to apply these actions in the context of `State.update`.

```elm
-- src/Todos.elm (line 46)
        |> mapCmd FormMsg
        |> andThen (\form -> save { state | form = form})
        |> consumeEvents
    AddItem item ->
      -- etc.
```

```elm
      state
        |> save
        |> andThen doStuff
        |> andThen doMoreStuff
```

Then, when the item is added, `onItemAdded` is invoked to pass the word to `Main`.

```elm
-- src/Todos.elm (line 49)
    AddItem item ->
      state
        |> pushItem item
        |> andInvoke events.onItemAdded
     -- etc.
```
