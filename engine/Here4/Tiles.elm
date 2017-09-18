module Here4.Tiles
    exposing
        ( Tiles
        , createTileGround
        )

import Array2D exposing (Array2D)
import Here4.Body exposing (Body)
import Here4.Ground exposing (..)
import Here4.Placement as Placement exposing (Placement)
import Math.Vector3 as V3 exposing (Vec3, vec3, getX, getZ)


type alias Tiles =
    { placement : Placement
    , elevations : Array2D Float
    , bodies : List Body
    }


createTileGround : Tiles -> ( Ground, List Body )
createTileGround tiles =
    let
        yMult = tiles.placement.yMult

        snowLevel = 0.8 * yMult
        beachLevel = 0.15 * yMult
        seaLevel = 0.1 * yMult
        deepSeaLevel = -0.4 * yMult

        surface pos =
            let
                h = tileElevation tiles pos
            in
                if h > snowLevel then
                    Snow
                else if h < deepSeaLevel then
                    DeepWater
                else if h <= seaLevel then
                    ShallowWater
                else if h < beachLevel then
                    Beach
                else
                    Grass
    in
        ( { bounds = tileBounds tiles
          , elevation = tileElevation tiles
          , nearestFloor = tileFloor tiles
          , seaLevel = seaLevel
          , surface = surface
          , coordRangeX = Placement.coordRangeX tiles.placement
          , coordRangeZ = Placement.coordRangeZ tiles.placement
          }
        , tiles.bodies
        )


tileBounds : Tiles -> Float -> Vec3 -> Vec3
tileBounds tiles radius pos =
    let
        { placement } =
            tiles

        bound x low high =
            if (x < low + radius) then
                low + radius
            else
                (if x > high - radius then
                    high - radius
                 else
                    x
                )

        ( x, y, z ) =
            V3.toTuple pos

        newX =
            let
                ( xMin, xMax ) =
                    Placement.coordRangeX placement
            in
                bound x (xMin + 10) (xMax - 10)

        newZ =
            let
                ( zMin, zMax ) =
                    Placement.coordRangeZ placement
            in
                bound z (zMin + 10) (zMax - 10)

        newY =
            bound y (tileElevation tiles (vec3 newX y newZ)) 1000
    in
        vec3 newX newY newZ



-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle


tileElevation : Tiles -> Vec3 -> Float
tileElevation { placement, elevations } pos =
    let
        ix0 =
            (getX pos - placement.xOffset) / placement.xDelta

        ix =
            floor ix0

        ixf =
            ix0 - toFloat ix

        iz0 =
            (getZ pos - placement.zOffset) / placement.zDelta

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


tileFloor : Tiles -> Vec3 -> Maybe Float
tileFloor tiles p =
    let
        e = tileElevation tiles p
        y = V3.getY p
    in
        if y >= e then
            Just (y - e)
        else
            Nothing
