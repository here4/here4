module Here4.Ground
    exposing
        ( Ground
        , nearestFloor
        , aboveSeaLevel
        , onWater
        , on
        )

import Here4.Barrier exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)


type alias Ground =
    { bounds : Float {- Radius -} -> Vec3 -> Vec3
    , barrier : Barrier
    , surface : Vec3 -> GroundSurface
    , elevation : Vec3 -> Float
    , seaLevel : Float
    , coordRangeX : ( Float, Float )
    , coordRangeZ : ( Float, Float )
    }


nearestFloor : Ground -> Vec3 -> Float
nearestFloor ground pos =
    let
        ray =
            { origin = pos
            , vector = vec3 0 -1 0
            }
    in
        ground.barrier ray
            |> Maybe.map (\b -> V3.distance pos b.position)
            |> Maybe.withDefault 1.0e7



-- eg. for use with AddApps.addSomewhere


aboveSeaLevel : Ground -> Vec3 -> Bool
aboveSeaLevel ground pos =
    ground.elevation pos > ground.seaLevel


onWater : Ground -> Vec3 -> Bool
onWater ground pos =
    ground.elevation pos <= ground.seaLevel


on : GroundSurface -> Ground -> Vec3 -> Bool
on surface ground pos =
    ground.surface pos == surface
