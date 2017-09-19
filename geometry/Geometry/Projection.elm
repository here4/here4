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


-- | Intersection point of the line containing (p0 -> p0+p)
-- onto the plane containing vector v with normal n
intersectPlane : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
intersectPlane v n p0 p =
    let
        d = V3.dot n p
        epsilon = 1e-6
    in
        if abs d  > epsilon then
            let
                w = V3.sub p0 v
                fac = (V3.dot n w / d)
                u = V3.scale fac p
            in
                Just (V3.add p0 u)
        else
            Nothing


-- Intersection of lines (u0 -> u0+u) and (v0 -> v0+v)
-- Adapated from
-- https://gist.github.com/hanigamal/6556506
intersectLineLine : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
intersectLineLine u0 u v0 v =
    let
        uv =
            V3.sub v0 u0

        uXv =
            V3.cross u v

        uXv2 =
            V3.dot uXv uXv

        epsilon =
            1e-6

        nearZero x =
            abs x < epsilon

        parallel =
            uXv2 == 0

        coplanar =
            nearZero (V3.dot uv uXv)

        s =
            (V3.dot (V3.cross uv v) uXv) / uXv2
    in
        -- if (coplanar && s >= 0.0 && s <= 1.0) then
        if not parallel && coplanar then
            Just (V3.add u0 (V3.scale s u))
        else
            Nothing


-- | Is the point p inside the quad (a,b,c,d) ?
insideQuad : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Bool
insideQuad a b c d p =
    let
        borders = [(a,b), (b,c), (c,d), (d,a)]
        inside (a,b) = V3.dot (V3.sub b a) (V3.sub p a) > 0
    in
        List.all inside borders

