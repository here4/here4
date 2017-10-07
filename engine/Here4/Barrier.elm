module Here4.Barrier
    exposing
        ( Ray
        , Quad(..)
        , intersectQuad
        , GroundSurface(..)
        , BarrierPoint
        , Barrier
        , joinBarriers
        , barrierFromQuad
        , barrierFromQuads
        , relativeBarrier
        )

import Geometry.Projection exposing (..)
import Here4.Orientation as Orientation exposing (Orientation)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import List.Extra as List
import Maybe.Extra as Maybe


type alias Ray =
    { origin : Vec3
    , vector : Vec3
    }


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


type GroundSurface
    = Snow
    | Grass
    | Beach
    | ShallowWater
    | DeepWater


type alias BarrierPoint =
    { position : Vec3
    , orientation : Orientation
    , surface : GroundSurface
    }


type alias Barrier =
    Ray -> Maybe BarrierPoint


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
            , orientation = Orientation.initial
            , surface = surface
            }
    in
        intersectQuad ray quad
        |> Maybe.map fromPosition


barrierFromQuads : GroundSurface -> List Quad -> Barrier
barrierFromQuads surface quads ray =
    List.map (\quad -> barrierFromQuad surface quad ray) quads
    |> Maybe.values
    |> List.minimumBy (\b -> V3.distanceSquared ray.origin b.position)


relativeBarrier : Vec3 -> Barrier -> Barrier
relativeBarrier relativePosition barrier ray =
    let
        relativeRay =
            { origin = V3.sub ray.origin relativePosition
            , vector = ray.vector
            }

        restore p =
            { p | position = V3.add p.position relativePosition }
    in
        Maybe.map restore (barrier relativeRay)

