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
        n_a = V3.normalize <| V3.cross (V3.sub b a) (V3.sub d a)
        n_c = V3.normalize <| V3.cross (V3.sub b c) (V3.sub d c)
        hitPoint_a = intersectPlane a n_a p0 p
        hitPoint_c = intersectPlane c n_c p0 p
        hitInside_a = insideTriangle a b d
        hitInside_c = insideTriangle c d b
    in
        Maybe.or
        (Maybe.filter hitInside_a hitPoint_a)
        (Maybe.filter hitInside_c hitPoint_c)


type GroundSurface
    = Snow
    | Grass
    | Beach
    | ShallowWater
    | DeepWater


type alias BarrierPoint =
    { position : Vec3
    , normal : Vec3
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
        (Quad a b c d) =
            quad

        n =
            V3.cross (V3.sub b a) (V3.sub d a)
            |> V3.normalize

        normal =
            if V3.dot n ray.vector > 0 then
                V3.negate n
            else
                n

        fromPosition p =
            { position = p
            , normal = normal
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

