namespace GameHelper

open FSharp.Reflection
open Elmish
open Browser
open Feliz
open Fable.Core

type Key =
| Digit0 | Digit1 | Digit2 | Digit3 | Digit4 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9
| KeyQ | KeyW | KeyE | KeyR | KeyT | KeyY | KeyU | KeyI | KeyO | KeyP
| KeyA | KeyS | KeyD | KeyF | KeyG | KeyH | KeyJ | KeyK | KeyL
| KeyZ | KeyX | KeyC | KeyV | KeyB | KeyN | KeyM
| ShiftLeft | ShiftRight | AltLeft | AltRight | ControlLeft | ControlRight
| Other of string

type Size = { Width:int; Height:int }

type InternalMessage =
| AnimationFrame of int

type Message =
| KeyDown of Key
| KeyUp of Key
/// Window resize message with new window size
| Resize of Size
/// new animation frame with elapsed time in ms
| Render of int
| Internal of InternalMessage

type Model = {
    PressedKeys: Set<Key>
    WindowSize: Size
    /// timestamp from last render frame
    LastRenderTimestamp: int
    /// ms elapsed since last render frame
    ElapsedTime: int
}

module Interop =
    type IResizeObserver =
        abstract observe : Types.HTMLElement -> unit
        abstract unobserve : Types.HTMLElement -> unit

    type IContentRect =
        abstract width : float
        abstract height : float
    type IObserverEntry =
        abstract contentRect : IContentRect

    [<Emit("new ResizeObserver($0)")>]
    let createResizeObserver(handler: IObserverEntry array -> unit) : IResizeObserver = jsNative

module Funcs =
    /// convert string to Key
    let private keyFromString (s:string) : Key=
        let info =
            FSharpType.GetUnionCases typeof<Key>
            |> Array.tryFind (fun case -> case.Name = s)
        match info with
        | Some case -> FSharpValue.MakeUnion(case,[||]) :?> Key
        | None -> Other s

    /// register keyboard- and window resize-events
    let private registerKeyboardEvents dispatch =
        let keyboardEvent msg text (event:Types.KeyboardEvent) =
            // printfn "%s: %A/%A %A %A %A %A %A" text event.code (keyFromString event.code) event.key event.shiftKey event.altKey event.ctrlKey event.metaKey
            event.code
            |> keyFromString
            |> msg
            |> dispatch
        
        document.onkeydown <- keyboardEvent Message.KeyDown "KeyDown"
        document.onkeyup <- keyboardEvent Message.KeyUp "KeyUp"
        
        window.requestAnimationFrame (
            int >> AnimationFrame >> Internal >> dispatch) |> ignore

    let init () : Model * Cmd<Message> =
        {   PressedKeys = Set.empty
            WindowSize = { Width = int window.innerWidth; Height = int window.innerHeight }
            LastRenderTimestamp = 0
            ElapsedTime = 0
        }, Cmd.ofSub registerKeyboardEvents

    let update (msg:Message) (model:Model) =
        match msg with
        | KeyDown key -> { model with PressedKeys = Set.add key model.PressedKeys }, Cmd.none
        | KeyUp key   -> { model with PressedKeys = Set.remove key model.PressedKeys }, Cmd.none
        | Resize size -> { model with WindowSize = size }, Cmd.none
        | Internal msg ->
            match msg with
            | AnimationFrame timestamp ->
                let elapsedTime = timestamp - model.LastRenderTimestamp
                let newModel =
                    { model with LastRenderTimestamp = timestamp; ElapsedTime = elapsedTime }
                let renderCmd = Cmd.ofSub (fun dispatch -> elapsedTime |> Render |> dispatch)
                let rafCmd = Cmd.ofSub (fun dispatch ->
                    window.requestAnimationFrame (
                        int >> AnimationFrame >> Internal >> dispatch) |> ignore)            
                newModel, Cmd.batch [ renderCmd; rafCmd]
        | Render _ -> model, Cmd.none

    /// Playfield is a div ReactElement with a ResizeObserver sending Resize messages
    [<ReactComponent>]
    let Playfield dispatch (props:IReactProperty list) : ReactElement =
        let observer = React.useRef(Interop.createResizeObserver(fun entries ->
                    entries
                    |> Array.tryHead
                    |> function
                    | None -> ()
                    | Some entry ->
                        // printfn "ResizeObserver: %A/%A" entry.contentRect.width entry.contentRect.height
                        { Width=int entry.contentRect.width; Height=int entry.contentRect.height}
                        |> Resize
                        |> dispatch
                )
            )

        let playfieldRef = React.useElementRef ()
        React.useEffect ((fun () ->
            match playfieldRef.current with
            | None -> ()
            | Some element -> observer.current.observe element
            React.createDisposable(fun () ->
                match playfieldRef.current with
                | None -> ()
                | Some element -> observer.current.unobserve element)), [| |])
        
        Html.div ([
            prop.ref playfieldRef
            // prop.tabIndex 1
            // prop.onKeyDown (fun event ->
            //     printfn "KeyDown %A" event.code
            //     event.code |> keyFromString |> Message.KeyDown |> dispatch)
            // prop.onKeyUp (fun event -> event.code |> keyFromString |> Message.KeyUp |> dispatch)
        ] @ props)