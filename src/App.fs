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
                        // svg.fill "red"
                        svg.fill "url(#grad1)"
                        svg.fillOpacity ship.ShieldStrength
                        
                    ]
                    Svg.polygon [
                        svg.points [
                            15, 5
                            8, 22
                            22, 22
                        ]
                        svg.fill ship.Color
                        // svg.stroke "black"
                        // svg.strokeWidth 0.1
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

    match msg with
    | GameHelper.Render timeStep ->
        let timeStep = (float timeStep)
        // printfn "fps: %f" (1000. / (timestamp - model.LastRenderTimestamp))
        let newShips = newModel.Ships |> List.map (processShipMovCmd model.Game.PressedKeys (timeStep / 10.))

        // simulation
        let newShips = newShips |> List.map (fun ship -> Simulation.simulateShip ship newModel.Game.WindowSize timeStep)
        let newBullets = newModel.Bullets |> Simulation.simulateBullets newModel.Game.WindowSize timeStep

        // simulate collisions
        let newShips, newBullets = CollisionDetection.simulateCollisions newShips newBullets

        { newModel with Ships = newShips; Bullets = newBullets }, cmd
    | GameHelper.KeyDown key ->
        let newBullets = newModel.Bullets @ (newModel.Ships |> processFireCmds key)
        { newModel with Bullets = newBullets}, cmd
    | _ -> newModel, cmd

let view (model:Model) dispatch =
    let fps = GameHelper.Funcs.framesPerSecond model.Game.RenderTimestamps
    let mspf = GameHelper.Funcs.msPerFrame model.Game.RenderTimestamps
    Html.div [
        prop.style [
            style.position.absolute
            style.top 20
            style.left 30
            style.width (length.calc "100vw - 60px")
        ]
        prop.children [
            Html.div [
                prop.style [ style.fontFamily "monospace"]
                prop.children [ Html.text "einszahn"]
            ]
            GameHelper.Funcs.Playfield dispatch [
                prop.style [
                    style.backgroundColor "grey"
                    style.position.relative
                    style.height (length.calc "100vh - 80px")
                    style.zIndex -1
                ]
                prop.children ([
                    Svg.svg [
                        Svg.defs [
                            Svg.radialGradient [
                                svg.id "grad1"
                                svg.cx 0.5
                                svg.cy 0.5
                                svg.r  0.9
                                svg.fx 0.5
                                svg.fy 0.5
                                svg.children [
                                    Svg.stop [
                                        svg.offset 0.3
                                        svg.stopColor "red"
                                        svg.stopOpacity 0.1
                                    ]
                                    Svg.stop [
                                        svg.offset 1.
                                        svg.stopColor "red"
                                        svg.stopOpacity 1.
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                @ (model.Ships |> List.map spaceShip)
                @ (model.Bullets |> List.map bullet))
            ]
            Html.div [
                prop.style [ style.fontFamily "monospace"]
                prop.children [ Html.text $"fps %.0f{fps} spf %.0f{mspf}ms"]
            ]
        ]
    ]

let init () =
    let gameModel, cmd = GameHelper.Funcs.init ()
    {   Ships = [
            Ship.init
            { Ship.init with
                CallSign = "ZweiZahn"
                Color    = "Green"
                Controls = { Forward=GameHelper.ArrowUp; Left=GameHelper.ArrowLeft; Right=GameHelper.ArrowRight; Fire=GameHelper.AltRight }
                Position = { X = 100.; Y = 100. }
                Rotation = 180.<deg> }
            { Ship.init with
                CallSign = "DreiZahn"
                Color    = "black"
                Controls = { Forward=GameHelper.Numpad8; Left=GameHelper.Numpad4; Right=GameHelper.Numpad6; Fire=GameHelper.Numpad0 }
                Position = { X = 200.; Y = 200. }
                Rotation = 270.<deg> }
        ]
        Bullets = List.empty
        Game = gameModel
    }, cmd

Program.mkProgram init update view
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "feliz-app"
|> Program.run