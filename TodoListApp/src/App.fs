module App

open Elmish
open Elmish.React
open Feliz

type Todo = {
  Id: int
  Description: string
  Completed: bool
}

type State = {
  TodoList : Todo list
  NewTodo : string
}

type Msg =
  | SetNewTodo of string
  | AddTodo
  | ToggleCompleted of int
  | DeleteTodo of int

let init() : State =
    { 
      TodoList = [
        { Id = 1; Description = "Learn F#"; Completed = true }
        { Id = 2; Description = "Learn Elmish"; Completed = false }
      ] 
      NewTodo = "" 
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

    | ToggleCompleted todoId -> 
        state
        
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

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList -> renderTodo todo dispatch
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