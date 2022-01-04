module App

open Elmish
open Elmish.React
open Feliz

let update msg model =
    model

let view model dispatch =
    Html.text "Hello world"
    
Program.mkSimple (fun _ -> "init") update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run