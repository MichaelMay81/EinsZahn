namespace MyMath

[<Measure>] type deg
[<Measure>] type rad

module angle =
    [<Literal>]
    let private Pi = 3.141592653589793<rad>

    let convertDegToRad (angle:float<deg>) : float<rad> =
        angle * Pi / 180.0<deg>
    let convertRadToDeg (angle:float<rad>) : float<deg> =
        angle / Pi * 180.0<deg>

    let radian (angle:float) : float<rad> = angle * 1.<rad>
    let degree (angle:float) : float<deg> = angle * 1.<deg>

    let sin (angle:float<rad>) = sin (float angle)
    let cos (angle:float<rad>) = cos (float angle)