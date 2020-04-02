module App

open Elmish
open Elmish.React
open Feliz

type State =
    { 
      Count: int 
      Loading : bool}

type Msg =
    | Increment
    | Decrement
    | IncrementDelayed
    | DecrementDelayed

let init() =
    { Count = 0 ; Loading = false }, Cmd.none

let update msg state =
    let delayedSleepMsg (sleep: Async<unit>) (msg: Msg) : Cmd<Msg> =
        let incrementDelayedCmd (dispatch: Msg -> unit) : unit =
            let delayedDispatch = async {
                do! sleep
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub incrementDelayedCmd

    let delayedMsg (delay: int) (msg: Msg) : Cmd<Msg> =
        delayedSleepMsg (Async.Sleep delay) msg      

    match msg with
    | Increment ->
        { state with Loading = false; Count = state.Count + 1 }, Cmd.none

    | Decrement ->
        { state with Count = state.Count - 1 }, Cmd.none

    | IncrementDelayed when state.Loading -> state, Cmd.none
    | IncrementDelayed -> state, delayedMsg 1000 Increment
    | DecrementDelayed -> state, delayedMsg 1000 Decrement

let render (state: State) (dispatch: Msg -> unit) =
  let headerText =
    if state.Count % 2 = 0
    then "Count is even"
    else "Count is odd"

  let content =
    if state.Loading
    then Html.h1 "LOADING..."
    else Html.h1 state.Count

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
      Html.button [
        prop.disabled state.Loading // <-- Disable the button if we click on it (state loading)
        prop.onClick (fun _ -> dispatch IncrementDelayed)
        prop.text "Increment Delayed"
      ]
      
      content

      Html.button [
        prop.onClick (fun _ -> dispatch Decrement)
        prop.text "Decrement"
      ]

      oddOrEvenMessage
      // if state.Count > 0 then yield oddOrEvenMessage // <=== if all is being yield.
      Html.p [
        prop.text "https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-to-cmd"
      ]
    ]
  ]

Program.mkProgram init update render // <-- mkProgram instead of mkSimple, will add a tuple for the command.
|> Program.withReactSynchronous "elmish-app"
|> Program.run