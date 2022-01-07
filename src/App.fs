module App

// EinsZahn

open FSharpPlus
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
                        svg.fill ship.Color
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
let bullet (bullet:Domain.Bullet) =
    Html.div [
        prop.style [
            style.position.absolute
            style.left -2
            style.top -10
        ]
        prop.children [
            Svg.svg [
                svg.transform [
                    transform.translate (bullet.Position.X, bullet.Position.Y)
                ]
                svg.width 4
                svg.height 4
                svg.children [
                    Svg.circle [
                        svg.cx 2
                        svg.cy 2
                        svg.r 2
                        svg.fill "black"
                    ]
                ]
            ]
        ]
    ]

let private processShipMovCmd (pressedKeys:Set<GameHelper.Key>) (timeStep:float) (ship:Ship) =
    [   ship.Controls.Forward, Simulation.ThrustForward
        ship.Controls.Left, Simulation.ThrustLeft
        ship.Controls.Right, Simulation.ThrustRight ]
    |> Seq.filter (fst >> flip Set.contains pressedKeys)
    |> Seq.map snd
    |> Seq.fold (fun newShip cmd ->
        Simulation.shipMovCmd newShip cmd timeStep) ship

let private processFireCmds (pressedKey:GameHelper.Key) (ships:Ship list) =
    ships
    |> Seq.filter (fun s -> s.Controls.Fire = pressedKey )
    |> Seq.map Simulation.shipFireCmd
    |> Seq.filter (fun bullet -> (abs bullet.Movement.X) > 0.1 || (abs bullet.Movement.Y) > 0.1)
    |> Seq.toList

let update msg (model:Model) =
    let gameModel, cmd = GameHelper.Funcs.update msg model.Game
    // printfn "%A" (pressedKeys |> Set.toList)
    let newModel = { model with Game = gameModel }

    let newModel = 
        match msg with
        | GameHelper.Render timeStep ->
            let timeStep = (float timeStep) / 10.
            // printfn "fps: %f" (1000. / (timestamp - model.LastRenderTimestamp))
            let newShips = newModel.Ships |> List.map (processShipMovCmd model.Game.PressedKeys timeStep)
            { newModel with Ships = newShips }
        | GameHelper.KeyDown key ->
            let newBullets = newModel.Bullets @ (newModel.Ships |> processFireCmds key)
            { newModel with Bullets = newBullets}
        | _ -> newModel

    // simulation
    let newShips = newModel.Ships |> List.map (fun ship -> Simulation.simulateShip ship newModel.Game.WindowSize model.Game.ElapsedTime)
    let newBullets = newModel.Bullets |> Simulation.simulateBullets newModel.Game.WindowSize newModel.Game.ElapsedTime
    { newModel with
        Ships = newShips
        Bullets = newBullets
    }, cmd

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
            prop.children
                ((model.Ships |> List.map spaceShip)
                @ (model.Bullets |> List.map bullet))
        ]
        Html.text "Hello world"
    ]

let init () =
    let gameModel, cmd = GameHelper.Funcs.init ()
    {   Ships = [
            Ship.init
            { Ship.init with
                CallSign = "ZweiZahn"
                Color    = "Green"
                Controls = { Forward=GameHelper.ArrowUp; Left=GameHelper.ArrowLeft; Right=GameHelper.ArrowRight; Fire=GameHelper.AltRight }
                Position = { X = 100.; Y = 100. } }
            { Ship.init with
                CallSign = "DreiZahn"
                Color    = "black"
                Controls = { Forward=GameHelper.Numpad8; Left=GameHelper.Numpad4; Right=GameHelper.Numpad6; Fire=GameHelper.Numpad0 }
                Position = { X = 200.; Y = 200. } }
        ]
        Bullets = List.empty
        Game = gameModel
    }, cmd

Program.mkProgram init update view
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run