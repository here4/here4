module Bounding.Box
    exposing
        ( Box
        , boundingBox
        )

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Time exposing (Time)
import Bounding exposing (..)


type alias Box =
    { position : Vec3 -- Position of box (0,0,0) coordinate
    --, orientation : Orientation
    , dimensions : Vec3
    }

boundingBox : Box -> Bounding Box
boundingBox box =
    { methods = methods
    , model = box
    }

methods : Methods Box
methods =
    { inside = inside
    , emplace = emplace
    , bounce = bounce
    , bump = bump
    }


inside : Box -> Vec3 -> Bool
inside box pos =
    let
        -- Relative position of point to box
        (rx, ry, rz) = V3.toTuple <| V3.sub pos box.position

        -- Dimensions of box
        (dx, dy, dz) = V3.toTuple box.dimensions
    in
        rx >= 0 && ry >= 0 && rz >= 0 && rx <= dx && ry <= dy && rz <= dz


emplace : Box -> Vec3 -> Vec3
emplace box pos =
    let
        -- Relative position of point to box
        (rx, ry, rz) = V3.toTuple <| V3.sub pos box.position

        -- Dimensions of box
        (dx, dy, dz) = V3.toTuple box.dimensions

        -- Clamp relative position to within dimensions
        nx = clamp 0 dx rx
        ny = clamp 0 dy ry
        nz = clamp 0 dz rz
    in
        V3.add (vec3 nx ny nz) box.position


bounce : Box -> Float -> Time -> Motion -> Motion
bounce box radius dt { position, velocity } =
    let
        -- Unbound relative position
        (rx, ry, rz) = V3.toTuple <| V3.sub (V3.add position (V3.scale dt velocity)) box.position

        -- Original velocity
        (vx, vy, vz) = V3.toTuple velocity

        -- Dimensions of box
        (dx, dy, dz) = V3.toTuple box.dimensions

        -- Reflect a one-dimensional (position, velocity) against walls at 0 and d
        bounce1 d (p, v) =
            if p < 0 then
                (-p, -v)
            else if (p > d) then
                (2*d - p, -v)
            else
                (p, v)

        -- Reflect relative position within dimensions
        (nx, nvx) = bounce1 dx (rx, vx)
        (ny, nvy) = bounce1 dy (ry, vy)
        (nz, nvz) = bounce1 dz (rz, vz)
    in
        { position = V3.add (vec3 nx ny nz) box.position
        , velocity = vec3 nvx nvy nvz
        }


bump : Box -> Float -> Time -> Motion -> Motion
bump box radius dt { position, velocity } =
    let
        -- Unbound relative position
        (rx, ry, rz) = V3.toTuple <| V3.sub (V3.add position (V3.scale dt velocity)) box.position

        -- Original velocity
        (vx, vy, vz) = V3.toTuple velocity

        -- Dimensions of box
        (dx, dy, dz) = V3.toTuple box.dimensions

        -- Bump a one-dimensional (position, velocity) against walls at 0 and d
        bump1 d (p, v) =
            if p < 0 then
                (0, 0)
            else if (p > d) then
                (d, 0)
            else
                (p, v)

        -- Bump relative position within dimensions
        (nx, nvx) = bump1 dx (rx, vx)
        (ny, nvy) = bump1 dy (ry, vy)
        (nz, nvz) = bump1 dz (rz, vz)
    in
        { position = V3.add (vec3 nx ny nz) box.position
        , velocity = vec3 nvx nvy nvz
        }
