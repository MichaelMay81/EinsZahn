module GameHelper

open Elmish
open Browser

type Message =
| KeyDown of string

let registerEvents dispatch =
    let foobar text (event:Types.KeyboardEvent) =
        // printfn "%s: %A %A %A" text event.char event.charCode event.key
        Message.KeyDown event.key |> dispatch
    // document.onkeypress <- foobar
    document.onkeydown <- foobar "KeyDown"
    document.onkeypress <- foobar "KeyPress"
    document.onkeyup <- foobar "KeyUp"