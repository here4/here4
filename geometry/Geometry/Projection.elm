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


-- | Projection of u onto the plane with normal n
projectPlaneNormal : Vec3 -> Vec3 -> Vec3
projectPlaneNormal n u =
    -- u, without its component that is not in the plane
    V3.sub u (V3.scale (V3.dot u n) n)


-- | Intersection point of the line containing p1, p2
-- onto the plane containing vector v with normal n
intersectPlane : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
intersectPlane v n p1 p2 =
    let
        ray = V3.sub p2 p1
        d = V3.dot n ray
        epsilon = 1e-6
    in
        if abs d  > epsilon then
            let
                w = V3.sub p1 v
                fac = -(V3.dot n w / d)
                u = V3.scale fac ray
            in
                Just (V3.add p1 u)
        else
            Nothing


-- Intersection of lines (u0 -> u0+u) and (v0 -> v0+v)
-- Adapated from
-- https://gist.github.com/hanigamal/6556506
intersectLineLine : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
intersectLineLine u0 u v0 v =
    let
        uv = V3.sub v0 u0

        uXv = V3.cross u v

        epsilon = 1e-6
        nearZero x = abs x < epsilon

        coplanar = nearZero (V3.dot uv uXv)

        s = (V3.dot (V3.cross uv v) uXv) / V3.dot uXv uXv
    in
        -- if (coplanar && s >= 0.0 && s <= 1.0) then
        if coplanar then
            Just (V3.add u0 (V3.scale s u))
        else
            Nothing
