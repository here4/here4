module Here4.SpaceTree exposing (..)


import Math.Vector3 as V3 exposing (Vec3)


type SpaceTree a
    = Leaf (List (SpaceBox, SpaceTree a))
    | X (List (Float, SpaceTree a))
    | Y (List (Float, SpaceTree a))
    | Z (List (Float, SpaceTree a))


type alias 

type alias SpaceBox =
    { position : Vec3 -- Position of box (0,0,0) coordinate
    , dimensions : Vec3
    }


insert : a -> SpaceBox -> SpaceTree a -> SpaceTree a



-- Find all elements that contain the given point
find  : Vec3 -> List a


----------------------------------------------------------------------
-- SpaceBox helpers


insideBox : SpaceBox -> Vec3 -> Bool
insideBox box pos =
    let
        -- Relative position of point to box
        ( rx, ry, rz ) =
            V3.toTuple <| V3.sub pos box.position

        -- Dimensions of box
        ( dx, dy, dz ) =
            V3.toTuple box.dimensions
    in
        rx >= 0 && ry >= 0 && rz >= 0 && rx <= dx && ry <= dy && rz <= dz

