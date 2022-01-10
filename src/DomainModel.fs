module Domain

open MyMath
open GameHelper
type Controls = {
    Forward: Key
    Left: Key
    Right: Key
    Fire: Key
} with static member init = { Forward=KeyW; Left=KeyA; Right=KeyD; Fire=Space }

type Ship = {
    CallSign: string
    Color: string
    Controls: Controls
    Position: Vector
    Rotation: float<deg>
    Movement: Vector
    ShieldStrength: float}
    with static member init = {
            CallSign = "EinsZahn"
            Color    = "blue"
            Controls = Controls.init
            Position = { X = 15.; Y = 15. }
            Rotation = 90.<deg>
            Movement = Vector.Zero
            ShieldStrength = 1.}

type Bullet = {
    Position: Vector
    Movement: Vector
}

type Model = {
    Ships: Ship list
    Bullets: Bullet list
    Game: GameHelper.Model
}