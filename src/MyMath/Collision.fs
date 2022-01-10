module MyMath.Collision

type Circle = {
    Position: Vector
    Radius: float
}

let doCirclesCollide (circle1:Circle) (circle2:Circle) : bool =
    let distance =
        (circle1.Position - circle2.Position)
        |> Vector.length
    let minDist = abs (circle1.Radius + circle2.Radius)
    distance < minDist