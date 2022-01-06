module App

open Elmish
open Elmish.React
open Feliz
open MyMath
open Domain

let spaceShip (ship:Domain.Ship) =
    Html.div [
        prop.style [
            style.position.absolute
            style.left -15
            style.top -15
        ]
        prop.children [
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
        ]
    ]

let update msg (model:Model) =
    let gameModel, cmd = GameHelper.Funcs.update msg model.Game
    // printfn "%A" (pressedKeys |> Set.toList)
    let newModel = { model with Game = gameModel }

    let newModel = 
        match msg with
        | GameHelper.Render timeStep ->
            let timeStep = (float timeStep) / 10.
            // printfn "fps: %f" (1000. / (timestamp - model.LastRenderTimestamp))
            let newShip =
                newModel.Game.PressedKeys
                |> Set.toList
                |> List.fold (fun newShip key ->
                    match key with
                    | GameHelper.KeyA ->
                        { newShip with Rotation = newShip.Rotation - (3.<deg> * timeStep)}
                    | GameHelper.KeyD ->
                        { newShip with Rotation = newShip.Rotation + (3.<deg> * timeStep)}
                    | GameHelper.KeyW ->
                        let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -newShip.Rotation)
                        let newPos = newShip.Position + (dir * timeStep)
                        // printfn "foobar %A %A %A" model.Rotation dir newPos
                        { newShip with Position = newPos }
                    | GameHelper.KeyS ->
                        let dir = Vector.rotate {X=0.;Y=1.} (convertDegToRad -newShip.Rotation) * -1.
                        let newPos = newShip.Position + (dir * timeStep)
                        // printfn "foobar %A %A %A" model.Rotation dir newPos
                        { newShip with Position = newPos }
                    | _ -> newShip
                    ) newModel.Ship

            { newModel with Ship = newShip}
        | _ -> newModel

    { newModel with Ship = Simulation.checkShipWorldBoundaries newModel.Ship newModel.Game.WindowSize }, cmd

let view (model:Model) dispatch =
    Html.div [
        GameHelper.Funcs.Playfield dispatch [
            prop.style [
                style.backgroundColor "grey"
                style.position.absolute
                style.top 50
                style.left 50
                style.height (length.calc "100vh - 100px")
                style.width (length.calc "100vw - 100px")
                // style.heigth (length.vh 100)
                //style.width (length.vw 100)
                style.zIndex -1
            ]
            prop.children [
                spaceShip model.Ship
            ]
        ]
        Html.text "Hello world"
    ]

let init () =
    let gameModel, cmd = GameHelper.Funcs.init ()
    {   Ship = Ship.init
        Game = gameModel
    }, cmd

Program.mkProgram init update view
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run