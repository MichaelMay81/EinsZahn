module App

open Elmish
open Elmish.React
open Feliz
open MyMath
open Domain

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

let update msg (model:Model) =
    let pressedKeys, cmd = GameHelper.update msg model.PressedKeys
    // printfn "%A" (pressedKeys |> Set.toList)
    let newModel = { model with PressedKeys = pressedKeys }

    let newModel = 
        match msg with
        | GameHelper.Render timestamp ->
            let timeStep = (timestamp - newModel.LastRenderTimestamp) / 10.
            // printfn "fps: %f" (1000. / (timestamp - model.LastRenderTimestamp))
            let newShip =
                pressedKeys
                |> Set.toList
                |> List.fold (fun newShip key ->
                    match key with
                    | GameHelper.KeyA ->
                        { newShip with Rotation = newShip.Rotation - (5.<deg> * timeStep)}
                    | GameHelper.KeyD ->
                        { newShip with Rotation = newShip.Rotation + (5.<deg> * timeStep)}
                    | GameHelper.KeyW ->
                        let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -newShip.Rotation)
                        let newPos = newShip.Position + (dir * timeStep)
                        // printfn "foobar %A %A %A" model.Rotation dir newPos
                        { newShip with Position = newPos } //{model.Position with Y = model.Position.Y + 1.}}
                    | GameHelper.KeyS ->
                        let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -newShip.Rotation) * -1.
                        let newPos = newShip.Position + (dir * timeStep)
                        // printfn "foobar %A %A %A" model.Rotation dir newPos
                        { newShip with Position = newPos } //{model.Position with Y = model.Position.Y - 1.}}
                    | _ -> newShip
                    ) newModel.Ship

            { newModel with LastRenderTimestamp = timestamp; Ship = newShip}
        | _ -> newModel

    // let newModel =
    //     match msg with
    //     | GameHelper.Message.KeyDown "a" ->
    //         { model with Rotation = model.Rotation - 5.<deg>}
    //     | GameHelper.Message.KeyDown "d" ->
    //         { model with Rotation = model.Rotation + 5.<deg>}
    //     | GameHelper.Message.KeyDown "w" ->
    //         let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -model.Rotation)
    //         let newPos = model.Position + dir
    //         // printfn "foobar %A %A %A" model.Rotation dir newPos
    //         { model with Position = newPos } //{model.Position with Y = model.Position.Y + 1.}}
    //     | GameHelper.Message.KeyDown "s" ->
    //         let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -model.Rotation) * -1.
    //         let newPos = model.Position + dir
    //         // printfn "foobar %A %A %A" model.Rotation dir newPos
    //         { model with Position = newPos } //{model.Position with Y = model.Position.Y - 1.}}
    //     | GameHelper.Message.KeyDown key ->
    //         model
    newModel, cmd

let view (model:Model) dispatch =
    Html.p [
        Html.text "Hello world"
        spaceShip model.Ship
    ]

let init () =
    let pressedKeys, cmd = GameHelper.init ()
    { Model.init with PressedKeys=pressedKeys }, cmd

Program.mkProgram init update view
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run