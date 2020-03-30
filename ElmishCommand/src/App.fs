module App

open Elmish
open Elmish.React
open Feliz

type State =
    { Count: int }

type Msg =
    | Increment
    | Decrement

let init() =
    { Count = 0 }, Cmd.none

let update msg state =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1 }, Cmd.none

    | Decrement ->
        { state with Count = state.Count - 1 }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
  let headerText =
    if state.Count % 2 = 0
    then "Count is even"
    else "Count is odd"

  let oddOrEvenMessage =  if state.Count > 0 then Html.h1 headerText else Html.none
  Html.div [
    prop.style [
      style.textAlign.center
    ]
    prop.children [
      Html.h1 [
          prop.style [ 
            // (if state.Count < 0 then style.display.none else style.display.block)
            state.Count < 0, [ style.display.none ] // Same as previous line
          ] 
          prop.text headerText
      ]      

      Html.button [
        prop.onClick (fun _ -> dispatch Increment)
        prop.text "Increment"
      ]
      
      Html.h1 state.Count

      Html.button [
        prop.onClick (fun _ -> dispatch Decrement)
        prop.text "Decrement"
      ]

      oddOrEvenMessage
      // if state.Count > 0 then yield oddOrEvenMessage // <=== if all is being yield.
    ]
  ]

Program.mkProgram init update render // <-- mkProgram instead of mkSimple, will add a tuple for the command.
|> Program.withReactSynchronous "elmish-app"
|> Program.run