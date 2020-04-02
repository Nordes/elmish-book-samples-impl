module App

open Elmish
open Elmish.React
open Feliz



type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type State = {
  RandomNumber : Deferred<Result<double, string>>
}

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't
    
type Msg =
  | GenerateRandomNumber of AsyncOperationStatus<Result<double, string>>

let init() =
    { RandomNumber = HasNotStartedYet }, Cmd.none

let rnd = System.Random()


module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

let update msg state =
    match msg with
    | GenerateRandomNumber Started when state.RandomNumber = InProgress -> state, Cmd.none

    | GenerateRandomNumber Started ->
        let randomOp : Async<Msg> = async {
          do! Async.Sleep 1000
          let random = rnd.NextDouble()
          if random > 0.5 then
            return GenerateRandomNumber (Finished (Ok random))
          else
            let errorMsg = sprintf "Failed! Random number %f was < 0.5" random
            return GenerateRandomNumber (Finished (Error errorMsg))
        }

        { state with RandomNumber = InProgress }, Cmd.fromAsync randomOp

    | GenerateRandomNumber (Finished result) ->
        let nextState = { state with RandomNumber = Resolved result }
        nextState, Cmd.none

    // Previous case cover the next 2.   
    // | GenerateRandomNumber (Finished (Ok randomNumber)) ->
    //     let nextState = { state with RandomNumber = Resolved (Ok randomNumber) }
    //     nextState, Cmd.none

    // | GenerateRandomNumber (Finished (Error error)) ->
    //     let nextState = { state with RandomNumber = Resolved (Error error) }
    //     nextState, Cmd.none


let render (state: State) (dispatch: Msg -> unit) =
  let content =
      match state.RandomNumber with
      | HasNotStartedYet ->
          Html.h1 "Hasn't started yet!"

      | InProgress ->
          Html.h1 "LOADING..."

      | Resolved (Ok number) ->
          Html.h1 [
              prop.style [ style.color.green ]
              prop.text (sprintf "Successfully generated random number: %f" number)
          ]

      | Resolved (Error errorMsg) ->
          Html.h1 [
              prop.style [ style.color.crimson ]
              prop.text errorMsg
          ]

  Html.div [
    prop.style [
      style.textAlign.center
    ]
    prop.children [
      Html.h1 [
          prop.text "Modeling Async State"
      ]      

      content
      // Html.span (sprintf "Hello %A" state.RandomNumber)

      Html.button [
        prop.onClick (fun _ -> dispatch (GenerateRandomNumber Started))
        prop.text "Generate random"
      ]

      // if state.Count > 0 then yield oddOrEvenMessage // <=== if all is being yield.
      Html.p [
        prop.text "https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state"
      ]
    ]
  ]

Program.mkProgram init update render // <-- mkProgram instead of mkSimple, will add a tuple for the command.
|> Program.withReactSynchronous "elmish-app"
|> Program.run