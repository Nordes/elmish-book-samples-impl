module App

open Elmish
open Elmish.React
open Feliz

type State = {
  TodoList : string list
  NewTodo : string
}

type Msg =
  | SetNewTodo of string
  | AddTodo

let init() : State =
    { TodoList = []; NewTodo = "" }

let update msg state =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }
    | AddTodo when state.NewTodo = "" -> state
    | AddTodo ->
        { state with
            NewTodo = ""
            TodoList = List.append state.TodoList [state.NewTodo] }

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

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList ->
        Html.li [
          prop.classes ["box"; "subtitle"]
          prop.text todo
        ]
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