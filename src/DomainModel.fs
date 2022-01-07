module Domain

open MyMath
open GameHelper
type Controls = {
    Forward: Key
    Left: Key
    Right: Key
} with static member init = { Forward=KeyW; Left=KeyA; Right=KeyD }

type Ship = {
    CallSign: string
    Color: string
    Controls: Controls
    Position: Vector
    Rotation: float<deg>
    Movement: Vector }
    with static member init = {
            CallSign = "EinsZahn"
            Color    = "blue"
            Controls = Controls.init
            Position = Vector.Zero
            Rotation = 0.<deg>
            Movement = Vector.Zero }

type Model = {
    Ships: Ship list
    Game: GameHelper.Model
}