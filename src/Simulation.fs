module Simulation

open MyMath
open GameHelper
open Domain

type ShipCmd =
| ThrustForward
| ThrustLeft
| ThrustRight

let checkWorldBoundaries (pos:Vector) (movement:Vector) (size:Size) =
    let width = float size.Width
    let height = float size.Height
    let x =
        if pos.X < 0. && movement.X <= 0. then
            width
        elif pos.X > width && movement.X >= 0. then
            0.
        else pos.X
    let y =
        if pos.Y < 0. && movement.Y <= 0. then
            height
        elif pos.Y > height && movement.Y >= 0. then
            0.
        else pos.Y
    { X = x; Y = y }

let shipMovCmd (ship:Ship) (cmd:ShipCmd) (timeStep:float) : Ship =
    match cmd with
    | ThrustRight ->
        { ship with Rotation = ship.Rotation + (3.<deg> * timeStep)}
    | ThrustLeft ->
        { ship with Rotation = ship.Rotation - (3.<deg> * timeStep)}
    | ThrustForward ->
        { ship with Movement = Vector.rotate {X=0.;Y=1.} (angle.convertDegToRad -ship.Rotation) }

let shipFireCmd (ship:Ship) : Bullet =
    let dir = Vector.rotate {X=0.;Y=1.} (angle.convertDegToRad -ship.Rotation)
    { Position = ship.Position + (dir * 20.); Movement = ship.Movement + dir }

let simulateShip (ship:Ship) (worldSize:Size) (timeStep:int) : Ship =
    let timeStep = (float timeStep) / 20.
    // move ship
    let newShip = { ship with Position = ship.Position + (ship.Movement * timeStep) }
    // check world boundaries
    { newShip with Position = checkWorldBoundaries newShip.Position newShip.Movement worldSize }

let simulateBullets (worldSize:Size) (timeStep:int) (bullets:Bullet list) : Bullet list =
    let timeStep = (float timeStep) / 15.
    bullets
    // move bullet
    |> Seq.map (fun bullet -> { bullet with Position = bullet.Position + (bullet.Movement*timeStep) })
    // remove bullets outside of world boundaries
    |> Seq.filter (fun bullet ->
        bullet.Position.X >= 0.
        && bullet.Position.X <= float worldSize.Width
        && bullet.Position.Y >= 0.
        && bullet.Position.Y <= float worldSize.Height)
    |> Seq.toList