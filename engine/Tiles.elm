module Tiles
    exposing
        ( Tiles
        , createTileGround
        )

import Math.Vector3 as V3 exposing (Vec3, vec3, getX, getZ)
import Array2D exposing (Array2D)
import Body exposing (Body)
import Ground exposing (Ground)
import Placement exposing (Placement)


type alias Tiles =
    { placement : Placement
    , elevations : Array2D Float
    , bodies : List Body
    }


createTileGround : Tiles -> ( Ground, List Body )
createTileGround tiles =
    ( { bounds = tileBounds tiles
      , elevation = tileElevation tiles
      }
    , tiles.bodies
    )


tileBounds : Tiles -> Vec3 -> Vec3
tileBounds { placement } pos =
    let
        bound x low high =
            if (x < low) then
                low
            else
                (if x > high then
                    high
                 else
                    x
                )

        ( x, y, z ) =
            V3.toTuple pos
    in
        vec3
            (bound x (placement.xOffset + 10) (placement.xOffset + toFloat placement.bigSide * placement.xDelta - 10))
            (bound y 0 1000)
            (bound z (placement.zOffset + 10) (placement.zOffset + toFloat placement.bigSide * placement.zDelta - 10))



-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle


tileElevation : Tiles -> Vec3 -> Float
tileElevation { placement, elevations } pos =
    let
        ix0 =
            (getX pos + 256) / 2

        ix =
            floor ix0

        ixf =
            ix0 - toFloat ix

        iz0 =
            (getZ pos + 256) / 2

        iz =
            floor iz0

        izf =
            iz0 - toFloat iz

        getXZ x z =
            (Array2D.getXY x z 0 elevations) * placement.yMult

        --     00 ... 10  -> x
        --  |  .    /  .
        --  v  .   /   .
        --     .  /    .
        --  z  01 ... 11
        i00 =
            getXZ ix iz

        i10 =
            getXZ (ix + 1) iz

        i01 =
            getXZ ix (iz + 1)

        i11 =
            getXZ (ix + 1) (iz + 1)

        mix a b f =
            (a * (1 - f) + b * f) / 2

        -- f describes how close to a
    in
        if ixf + izf < 1.0 then
            mix i00 i10 ixf + mix i00 i01 izf
        else
            mix i01 i11 ixf + mix i10 i11 izf
