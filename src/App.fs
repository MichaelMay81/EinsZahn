module App

open Elmish
open Elmish.React
open Feliz
open MyMath

let spaceShip (ship:Domain.Ship) =
    Svg.svg [
        svg.transform [
            transform.translate (ship.Position.X, ship.Position.Y)
            transform.rotate (float ship.Rotation)
        ]
        svg.width 30
        svg.height 30
        svg.children [
            Svg.circle [
                svg.cx 15
                svg.cy 15
                svg.r 15
                svg.fill "blue"
            ]
            Svg.polygon [
                svg.points [
                    15, 0
                    5, 25
                    25, 25
                ]
                svg.fill "red"
            ]
        ]
    ]

let update msg (model:Domain.Ship) =
    let newModel =
        match msg with
        | GameHelper.Message.KeyDown "a" ->
            { model with Rotation = model.Rotation - 5.<deg>}
        | GameHelper.Message.KeyDown "d" ->
            { model with Rotation = model.Rotation + 5.<deg>}
        | GameHelper.Message.KeyDown "w" ->
            let dir = rotate {X=0.;Y=1.} (convertDegToRad -model.Rotation)
            let newPos = add model.Position dir
            // printfn "foobar %A %A %A" model.Rotation dir newPos
            { model with Position = newPos } //{model.Position with Y = model.Position.Y + 1.}}
        | GameHelper.Message.KeyDown "s" ->
            let dir = mul (rotate {X=0.;Y=1.} (convertDegToRad -model.Rotation)) -1.
            let newPos = add model.Position dir
            // printfn "foobar %A %A %A" model.Rotation dir newPos
            { model with Position = newPos } //{model.Position with Y = model.Position.Y - 1.}}
        | GameHelper.Message.KeyDown key ->
            model
    newModel, Cmd.none

let view model dispatch =
    Html.p [
        Html.text "Hello world"
        spaceShip model
    ]

let init () =
    Domain.Ship.Default, Cmd.ofSub GameHelper.registerEvents

// Program.mkSimple (fun _ -> "init") update view
Program.mkProgram init update view
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run