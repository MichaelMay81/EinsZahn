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
    PressedKeys: GameHelper.PressedKeys
    LastRenderTimestamp: float
} with static member init = { Ship = Ship.init; PressedKeys = Set.empty; LastRenderTimestamp = 0. }