module App

open Elmish
open Elmish.React
open Feliz

type Todo = {
  Id: int
  Description: string
  Completed: bool
}

type TodoBeingEdited = {
  Id: int
  Description: string
}

type State = {
  NewTodo : string
  TodoList : Todo list
  TodoBeingEdited : TodoBeingEdited option
}

type Msg =
  | SetNewTodo of string
  | AddTodo
  | ToggleCompleted of int
  | DeleteTodo of int
  | CancelEdit
  | ApplyEdit
  | StartEditingTodo of int
  | SetEditedDescription of string

let init() : State =
    { 
      NewTodo = "" 
      TodoList = [
          { Id = 1; Description = "Learn F#"; Completed = true }
          { Id = 2; Description = "Learn Elmish"; Completed = false }
        ] 
      TodoBeingEdited = None 
    }

let update msg state =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }

    | DeleteTodo todoId ->
        let nextTodoList =
          state.TodoList
          |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

    | ToggleCompleted todoId ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo ->
           if todo.Id = todoId
           then { todo with Completed = not todo.Completed }
           else todo)

      { state with TodoList = nextTodoList }
  
    | AddTodo when state.NewTodo = "" -> state

    | AddTodo ->
      let nextTodoId =
        match state.TodoList with
        | [ ] -> 1
        | elems ->
            elems
            |> List.maxBy (fun todo -> todo.Id)
            |> fun todo -> todo.Id + 1

      let nextTodo =
        { Id = nextTodoId
          Description = state.NewTodo
          Completed = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

    | StartEditingTodo todoId ->
        let nextEditModel =
          state.TodoList
          |> List.tryFind (fun todo -> todo.Id = todoId)
          |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })

        { state with TodoBeingEdited = nextEditModel }

    | CancelEdit ->
        { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
              state.TodoList
              |> List.map (fun todo ->
                  if todo.Id = todoBeingEdited.Id
                  then { todo with Description = todoBeingEdited.Description }
                  else todo)

            { state with TodoList = nextTodoList; TodoBeingEdited = None }

    | SetEditedDescription newText ->
        let nextEditModel =
          state.TodoBeingEdited
          |> Option.map (fun todoBeingEdited -> { todoBeingEdited with Description = newText })

        { state with TodoBeingEdited = nextEditModel }    
        
/// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]

let todoInputField (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded"]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault state.NewTodo
            prop.onTextChange (SetNewTodo >> dispatch)
          ]
        ]
      ]

      Html.div [
        prop.className "control"
        prop.children [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch AddTodo)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column" ] [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
          prop.onClick (fun _ -> dispatch (StartEditingTodo  todo.Id))
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.className [ true, "button"; todo.Completed, "is-success"]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]
          
          Html.button [
            prop.classes [ "button"; "is-primary" ]
            prop.onClick (fun _ -> dispatch (StartEditingTodo  todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]
let renderEditForm (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "field"; "is-grouped" ] [
      div [ "control"; "is-expanded" ] [
        Html.input [
          prop.autoFocus true
          prop.classes [ "input"; "is-medium" ]
          prop.valueOrDefault todoBeingEdited.Description;
          prop.onTextChange (SetEditedDescription >> dispatch)
        ]
      ]

      div [ "control"; "buttons" ] [
        Html.button [
          prop.classes [ "button"; "is-primary"]
          prop.onClick (fun _ -> dispatch ApplyEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-save" ] ]
          ]
        ]

        Html.button [
          prop.classes ["button"; "is-warning"]
          prop.onClick (fun _ -> dispatch CancelEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-arrow-right"] ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList ->
        match state.TodoBeingEdited with
        | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
          renderEditForm todoBeingEdited dispatch
        | _ ->
          renderTodo todo dispatch      
    ]
  ]

let appFooter =
  Html.footer [
    prop.style [
      style.fontSize 12
      style.textAlign.center
      style.paddingTop 15
    ]
    prop.children [
      Html.hr []
      Html.text "Ref.: "
      Html.a [
        prop.text "The elmish book"
        prop.href "https://zaid-ajaj.github.io/the-elmish-book/#/chapters/elm/todo-app-part1"
      ]
    ]
  ]
let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do List"
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    appTitle

    Html.div [
      prop.style [ style.margin 50 ]
      prop.children [
        todoInputField state dispatch
        todoList state dispatch
      ]
    ]

    appFooter
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run