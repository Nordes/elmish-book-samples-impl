module App

open Elmish
open Elmish.React
open Feliz

type State = { 
  TextInput : string 
  NumberInput : int
  }

type Msg =
    | SetTextInput of string
    | SetNumberInput of int

let init() =
    { TextInput = ""; NumberInput = 0 }

let update (msg: Msg) (state: State): State =
    match msg with
    | SetTextInput newVal ->
        { state with TextInput = newVal }
    | SetNumberInput number ->
        { state with NumberInput = number } // Missing validation

let render (state: State) (dispatch: Msg -> unit) =
  let renderFooter =
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
          prop.href "https://zaid-ajaj.github.io/the-elmish-book/#/chapters/elm/form-inputs"
        ]
      ]
    ]
  let noValidationInputMessage = 
    Html.div [
      Html.h3 "No validation input"
      Html.br []
      Html.input [
        prop.onChange (SetTextInput >> dispatch) // Dispatch the change event.
      ]

      Html.span state.TextInput  
    ]

  let intValidationInputMessage = 
    Html.div [
      Html.h3 "Integer validation input"
      Html.br []
      Html.input [
        prop.type'.number
        prop.valueOrDefault state.NumberInput
        prop.onChange (int >> SetNumberInput >> dispatch) 
      ]

      Html.span state.NumberInput  
      
    ]

  Html.div [
    Html.h1 [
      prop.style [
        style.textAlign.center
      ]
      prop.children [
        Html.text "Collecting user input"
      ]
    ]

    noValidationInputMessage
    Html.hr [ prop.style [ style.width 500 ] ]
    intValidationInputMessage

    renderFooter
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run