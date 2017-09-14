module Geometry.Projection exposing (..)

import Math.Vector3 as V3 exposing (Vec3)


-- | Projection of u onto the plane containing vectors v1, v2
projectPlane : Vec3 -> Vec3 -> Vec3 -> Vec3
projectPlane v1 v2 u =
    let
        -- normal vector to the plane containing v1, v2
        n =
            V3.normalize <| V3.cross v1 v2
    in
        -- u, without its component that is not in the plane
        V3.sub u (V3.scale (V3.dot u n) n)

