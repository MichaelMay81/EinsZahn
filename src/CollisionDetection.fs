module CollisionDetection

open FSharpPlus
open Domain
open MyMath.Collision

let simulateCollisions (ships: Ship list) (bullets: Bullet list) : (Ship list) * (Bullet list) =
    match bullets |> List.isEmpty with
    | true -> ships, bullets
    | false ->
        let hitShips, hitBullets =
            bullets
            |> Seq.map (fun bullet -> ships
                                      |> Seq.tryFind (fun ship ->
                                            doCirclesCollide
                                                { Position = ship.Position; Radius = 15. }
                                                { Position = bullet.Position; Radius = 2. })
                                      |> Option.map (fun ship -> ship, bullet))
            |> Seq.choose id
            |> Seq.toList
            |> List.unzip

        let newBullets =
            bullets
            |> Seq.filter (not << (flip Seq.contains hitBullets))
            |> Seq.toList
        
        let hitShips =
            hitShips
            |> Seq.groupBy id
            |> Seq.map (fun (ship, ships) -> ship, ships |> Seq.length)
            |> dict

        let newShips =
            ships
            |> Seq.map (fun ship ->
                        match ship |> flip Dict.tryGetValue hitShips with
                        | None -> ship
                        | Some hits ->
                            { ship with ShieldStrength = ship.ShieldStrength - (0.4 * float hits) })
            |> Seq.filter (fun ship -> ship.ShieldStrength > -0.4)
            |> Seq.toList

        newShips, newBullets