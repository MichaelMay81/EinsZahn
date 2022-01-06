module Domain

open MyMath

type Ship = {
    Position: Vector
    Rotation: float<deg>
    Movement: Vector }
    with static member init = {
            Position = Vector.Zero
            Rotation = 0.<deg>
            Movement = Vector.Zero }

type Model = {
    Ship: Ship
    Game: GameHelper.Model
}