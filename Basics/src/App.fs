module App

open Browser.Dom // Access to JS dom objects. [Fable binding]

let incrementButton = document.getElementById "increment"
let decrementButton = document.getElementById "decrement"
let increaseDelayedBtn = document.getElementById "increaseDelayed"
let countViewer = document.getElementById "countViewer"

let mutable currentCount = 0 // Bad practice : Mutable
let rnd = System.Random()

let setCountText currentCount =
    // set the count viewer with the initial count
    countViewer.innerText <- sprintf "Count is at %d" currentCount

// attach event handlers
incrementButton.onclick <- fun ev ->
    // update the state
    currentCount <- currentCount + rnd.Next(5, 10)
    // update the view
    setCountText currentCount

decrementButton.onclick <- fun ev ->
    // update the state
    currentCount <- currentCount -  rnd.Next(5, 10)
    // update the view
    setCountText currentCount

// Runs the callback after a delay
let runAfter ms callback =
  async {
    do! Async.Sleep ms
    do callback()
  }
  |> Async.StartImmediate

increaseDelayedBtn.onclick <- fun _ ->
  runAfter 1000 (fun () ->
    currentCount  <- currentCount  + rnd.Next(5, 10)
    setCountText currentCount
  )

// set the count viewer with the initial count
setCountText currentCount
