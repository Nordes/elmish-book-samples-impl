module App

open Browser.Dom // Access to JS dom objects. [Fable binding]

// https://zaid-ajaj.github.io/the-elmish-book/#/chapters/fable/hello-world
printfn "Fable is up and running..."

let printMsgButton = document.getElementById "printMsg"

printMsgButton.onclick <- fun eventArgs ->
    printfn "Button clicked"

