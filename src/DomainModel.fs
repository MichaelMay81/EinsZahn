module Domain

open MyMath

type Ship = {
    Position: Vector
    Rotation: float<deg>
    Movement: Vector }
    with static member Default = {
            Position = Vector.Zero
            Rotation = 0.<deg>
            Movement = Vector.Zero }

type Model = {
    Ship: Ship
    PressedKeys: GameHelper.PressedKeys
}