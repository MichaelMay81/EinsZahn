module Simulation

open MyMath
open GameHelper
open Domain

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

let checkShipWorldBoundaries (ship:Ship) (size:Size) : Ship =
    { ship with Position = checkWorldBoundaries ship.Position ship.Movement size }