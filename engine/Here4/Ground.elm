module Here4.Ground
    exposing
        ( Ground
        , GroundSurface(..)
        , Ray
        , Quad(..)
        , nearestFloor
        , intersectQuad
        , BarrierPoint
        , Barrier
        , joinBarriers
        , barrierFromQuads
        , relativeBarrier
        , aboveSeaLevel
        , onWater
        , on
        )


import Geometry.Projection exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import List.Extra as List -- barrier
import Maybe.Extra as Maybe -- barrier
import Here4.Orientation as Orientation exposing (Orientation)


type GroundSurface
    = Snow
    | Grass
    | Beach
    | ShallowWater
    | DeepWater


type alias Ray =
    { origin : Vec3
    , vector : Vec3
    }


type alias BarrierPoint =
    { position : Vec3
    -- , orientation : Orientation
    , surface : GroundSurface
    }


type alias Barrier =
    Ray -> Maybe BarrierPoint


type alias Floor =
    Vec3 -> Maybe Float


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
        |> Maybe.withDefault 1e7


type Quad =
    Quad Vec3 Vec3 Vec3 Vec3


intersectQuad : Ray -> Quad -> Maybe Vec3
intersectQuad ray (Quad a b c d) =
    let
        p0 = ray.origin
        p = ray.vector
        n = V3.normalize <| V3.cross (V3.sub b a) (V3.sub d a)
        hitPoint = intersectPlane a n p0 p
        hitInsideQuad = insideQuad a b c d
    in
        Maybe.filter hitInsideQuad hitPoint


joinBarriers : List Barrier -> Barrier
joinBarriers barriers ray =
    let
        distance2 ray point =
            V3.distanceSquared ray.origin point.position
    in
        List.map (\f -> f ray) barriers
        |> Maybe.values
        |> List.minimumBy (distance2 ray)


barrierFromQuad : GroundSurface -> Quad -> Barrier
barrierFromQuad surface quad ray =
    let
        fromPosition p =
            { position = p
            , surface = surface
            }
    in
        intersectQuad ray quad
        |> Maybe.map fromPosition


barrierFromQuads : GroundSurface -> List Quad -> Barrier
barrierFromQuads surface quads ray =
    let
        fromPosition p =
            { position = p
            , surface = surface
            }
    in
        List.map (intersectQuad ray) quads
        |> Maybe.values
        |> List.minimumBy (V3.distanceSquared ray.origin)
        |> Maybe.map fromPosition


relativeBarrier : Vec3 -> Barrier -> Barrier
relativeBarrier relativePosition barrier ray =
    let
        relativeRay =
            { origin = V3.sub ray.origin relativePosition
            , vector = ray.vector
            }
    in
        barrier relativeRay

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
