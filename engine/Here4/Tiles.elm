module Here4.Tiles
    exposing
        ( Tiles
        , createTileGround
        )

import Array2D exposing (Array2D)
import Geometry.Projection exposing (..)
import Here4.Body exposing (Body)
import Here4.Ground exposing (..)
import Here4.Placement as Placement exposing (Placement)
import Math.Vector3 as V3 exposing (Vec3, vec3, getX, getZ)
import Maybe.Extra as Maybe -- barrier


type alias Tiles =
    { placement : Placement
    , elevations : Array2D Float
    , bodies : List Body
    }


createTileGround : Tiles -> ( Ground, List Body )
createTileGround tiles =
    let
        yMult = tiles.placement.yMult
        seaLevel = 0.1 * yMult
    in
        ( { bounds = tileBounds tiles
          , elevation = tileElevation tiles
          , barrier = tileBarrier tiles
          , seaLevel = seaLevel
          , surface = tileSurface tiles
          , coordRangeX = Placement.coordRangeX tiles.placement
          , coordRangeZ = Placement.coordRangeZ tiles.placement
          }
        , tiles.bodies
        )


tileSurface : Tiles -> Vec3 -> GroundSurface
tileSurface tiles pos =
    surfaceAtElevation tiles (tileElevation tiles pos)


surfaceAtElevation : Tiles -> Float -> GroundSurface
surfaceAtElevation tiles h =
    let
        yMult = tiles.placement.yMult

        snowLevel = 0.8 * yMult
        beachLevel = 0.15 * yMult
        seaLevel = 0.1 * yMult
        deepSeaLevel = -0.4 * yMult
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


tileBarrier : Tiles -> Barrier
tileBarrier tiles ray =
    let
        fromPosition p =
            { position = p
            , surface = surfaceAtElevation tiles (V3.getY p)
            }
    in
        List.map (quadAt tiles) (nearbyIndices tiles.placement ray)
        |> List.map (intersectQuad ray)
        |> Maybe.values
        |> List.head
        |> Maybe.map fromPosition


nearbyIndices : Placement -> Ray -> List (Int, Int)
nearbyIndices placement ray =
    let
        ix0 =
            (getX ray.origin - placement.xOffset) / placement.xDelta

        ix =
            floor ix0

        iz0 =
            (getZ ray.origin - placement.zOffset) / placement.zDelta

        iz =
            floor iz0

        vx =
            V3.getX ray.vector

        vz =
            V3.getZ ray.vector

        sgn v =
            if abs v < 1e-3 then
                0
            else if v < 0 then
                -1
            else
                1

        xrange d =
            if abs vx < 1e-3 then
                [ix]
            else if vx < 0 then
                List.range (max 0 (ix-d)) ix
            else
                List.range ix (min placement.tileSize (ix+d))

        zrange d =
            if abs vz < 1e-3 then
                [iz]
            else if vz < 0 then
                List.range (max 0 (iz-d)) iz
            else
                List.range iz (min placement.tileSize (iz+d))

        rowAtDistance d =
            List.map (\x -> (x, iz + (d * sgn vz))) (xrange d)

        colAtDistance d =
            List.map (\z -> (ix + (d * sgn vx), z)) (zrange d)

        radius d =
            rowAtDistance d ++ colAtDistance d

        maxXD =
            if abs vx < 1e-3 then
                0
            else if vx < 0 then
                ix
            else
                placement.tileSize - ix - 1

        maxZD =
            if abs vz < 1e-3 then
                0
            else if vz < 0 then
                iz
            else
                placement.tileSize - iz - 1

        maxRadius =
            max maxXD maxZD
    in
        List.concatMap radius (List.range 0 maxRadius)


quadAt : Tiles -> (Int, Int) -> Quad
quadAt { placement, elevations } (ix, iz) =
    let
        getY x z =
            (Array2D.getXY x z 0 elevations) * placement.yMult

        getX x =
            toFloat x * placement.xDelta + placement.xOffset

        getZ z =
            toFloat z * placement.zDelta + placement.zOffset

        pos x z =
            vec3 (getX x) (getY x z) (getZ z)
    in
        Quad (pos ix iz) (pos (ix+1) iz) (pos (ix+1) (iz+1)) (pos ix (iz+1))
