module GameHelper

open FSharp.Reflection
open Elmish
open Browser

type Key =
| Digit0 | Digit1 | Digit2 | Digit3 | Digit4 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9
| KeyQ | KeyW | KeyE | KeyR | KeyT | KeyY | KeyU | KeyI | KeyO | KeyP
| KeyA | KeyS | KeyD | KeyF | KeyG | KeyH | KeyJ | KeyK | KeyL
| KeyZ | KeyX | KeyC | KeyV | KeyB | KeyN | KeyM
| ShiftLeft | ShiftRight | AltLeft | AltRight | ControlLeft | ControlRight
| Other of string

type Message =
| KeyDown of Key
| KeyUp of Key

type PressedKeys = Set<Key>

let private fromString (s:string) : Key=
    let info =
        FSharpType.GetUnionCases typeof<Key>
        |> Array.tryFind (fun case -> case.Name = s)
    match info with
    | Some case -> FSharpValue.MakeUnion(case,[||]) :?> Key
    | None -> Other s

let private registerEvents dispatch =
    let foobar msg text (event:Types.KeyboardEvent) =
        // printfn "%s: %A/%A %A %A %A %A %A" text event.code (fromString event.code) event.key event.shiftKey event.altKey event.ctrlKey event.metaKey
        event.code
        |> fromString
        |> msg
        |> dispatch
    
    document.onkeydown <- foobar Message.KeyDown "KeyDown"
    //document.onkeypress <- foobar "KeyPress"
    document.onkeyup <- foobar Message.KeyUp "KeyUp"

let update (msg:Message) (model:PressedKeys) =
    match msg with
    | KeyDown key -> Set.add key model
    | KeyUp key   -> Set.remove key model

let init () =
    Set.empty, Cmd.ofSub registerEvents