namespace GameHelper

open FSharp.Reflection
open FSharpPlus
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
| ArrowUp | ArrowDown | ArrowLeft | ArrowRight
| Numpad0 | Numpad1 | Numpad2 | Numpad3 | Numpad4 | Numpad5 | Numpad6 | Numpad7 | Numpad8 | Numpad9
| Space | Tab
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
    /// timestamp from last several render frames
    RenderTimestamps: int list
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
    let elapsedTime (timestamps: int list) : int =
        let ts1 = timestamps |> List.tryHead
        let ts2 = timestamps |> List.tryItem 1
        match ts1, ts2 with
        | Some ts1, Some ts2 -> ts1 - ts2
        | _, _ -> 0

    /// calculates the average ms per frame
    let msPerFrame (timestamps:int list) : float =
        match timestamps |> List.length < 2 with
        | true -> -1.
        | false ->
            timestamps
            |> Seq.pairwise
            |> Seq.map (fun (ts1,ts2) -> ts1 - ts2)
            |> Seq.map float
            |> Seq.average

    /// calculate the averae frames per second
    let framesPerSecond (timestamps:int list) : float =
        match timestamps |> List.isEmpty with
        | true -> -1.
        | false -> 1000. / msPerFrame timestamps

    /// convert string to Key
    let private keyFromString (s:string) : Key=
        let info =
            FSharpType.GetUnionCases typeof<Key>
            |> Array.tryFind (fun case -> case.Name = s)
        match info with
        | Some case -> FSharpValue.MakeUnion(case,[||]) :?> Key
        | None ->
            printfn "Key NIY: %s" s
            Other s

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
            RenderTimestamps = List.empty
        }, Cmd.ofSub registerKeyboardEvents

    let update (msg:Message) (model:Model) =
        match msg with
        | KeyDown key -> { model with PressedKeys = Set.add key model.PressedKeys }, Cmd.none
        | KeyUp key   -> { model with PressedKeys = Set.remove key model.PressedKeys }, Cmd.none
        | Resize size -> { model with WindowSize = size }, Cmd.none
        | Internal msg ->
            match msg with
            | AnimationFrame timestamp ->
                let newTimestamps =
                    timestamp :: model.RenderTimestamps
                    |> List.filter (fun ts -> timestamp - ts < 1000)
                let elTime = elapsedTime newTimestamps

                let renderCmd = Cmd.ofSub (fun dispatch -> elTime |> Render |> dispatch)
                let rafCmd = Cmd.ofSub (fun dispatch ->
                    window.requestAnimationFrame (
                        int >> AnimationFrame >> Internal >> dispatch) |> ignore)  
                
                { model with RenderTimestamps = newTimestamps }, Cmd.batch [ renderCmd; rafCmd]
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