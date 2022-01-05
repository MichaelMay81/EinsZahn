module MyMath

open System
open FSharp.Data.UnitSystems.SI

[<Measure>] type deg
[<Measure>] type rad

[<Literal>]
let private Pi = 3.141592653589793<rad>

let convertDegToRad (angle:float<deg>) : float<rad> =
    angle * Pi / 180.0<deg>
let convertRadToDeg (angle:float<rad>) : float<deg> =
    angle / Pi * 180.0<deg>

let radian (angle:float) : float<rad> = angle * 1.<rad>
let degree (angle:float) : float<deg> = angle * 1.<deg>

let sin (angle:float<rad>) = Math.Sin (float angle)
let cos (angle:float<rad>) = Math.Cos (float angle)

type Vector = { X:float; Y:float } with
    static member Zero = { X=0.; Y=0. }
    static member Unit = { X=1.; Y=1. }

let add vec1 vec2 =
    { X = vec1.X + vec2.X; Y = vec1.Y + vec2.Y}

let mul vec value =
    { X = vec.X * value; Y = vec.Y * value }

let rotate vec (angle:float<rad>) =
    {
        X = (cos angle) * vec.X - (sin angle) * vec.Y
        Y = (sin angle) * vec.X - (cos angle) * vec.Y
    }