module Body.Terrain
    exposing
        ( generate
        , step
        , paint
        , ripplePaint
        , mountains
        , sea
        )

import Math.Matrix4 as M4
import Math.Vector3 as V3
import Math.Vector4 as V4
import Random
import Array
import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (..)
import Math.Procedural exposing (..)
import Appearance exposing (..)
import Body exposing (Body, Oriented, Visible, toBody)
import Ground exposing (Ground)
import Orientation
import Placement exposing (Placement, defaultPlacement)
import Tiles exposing (createTileGround)
import Body.Surface2D exposing (..)


----------------------------------------------------------------------


generate : (( Ground, List Body ) -> msg) -> Cmd msg
generate tagger =
    generateWithPlacement defaultPlacement tagger


generateWithPlacement : Placement -> (( Ground, List Body ) -> msg) -> Cmd msg
generateWithPlacement placement tagger =
    Random.generate tagger (randTerrain placement)


step : Random.Seed -> ( ( Ground, List Body ), Random.Seed )
step =
    stepWithPlacement defaultPlacement


stepWithPlacement : Placement -> Random.Seed -> ( ( Ground, List Body ), Random.Seed )
stepWithPlacement placement seed =
    Random.step (randTerrain placement) seed


createWithPlacement : Placement -> Array2D Float -> ( Ground, List Body )
createWithPlacement placement elevations =
    let
        terrain =
            { placement = placement
            , elevations = elevations
            , bodies =
                paint mountains placement elevations
                    ++ ripplePaint sea 0.3 placement elevations
            }
    in
        createTileGround terrain


randTerrain : Placement -> Random.Generator ( Ground, List Body )
randTerrain placement =
    let
        elevations =
            randTerrain2D (placement.bigSide + 1)
    in
        Random.map (createWithPlacement placement) elevations



----------------------------------------------------------------------


mountains : Float -> NoiseSurfaceVertex
mountains h =
    let
        green =
            hslToVec3
                (degrees (70 + toFloat (round ((h + 0.34) * 500) % 70)))
                (0.3 + h / 4)
                (0.2 + (1 - h) / 3)

        sand =
            hslToVec3 (degrees 50) 0.8 ((h + 0.1) * 4)

        seafloor =
            hslToVec3 (degrees 50) 0.3 ((h + 0.1) * 4)

        snow =
            hslToVec3 (degrees 178) 0.8 h

        alpha1 v0 =
            let
                v =
                    V3.toRecord v0
            in
                V4.fromTuple ( v.x, v.y, v.z, 1.0 )
    in
        if h > 0.8 then
            ( h, alpha1 snow, 0.8, 0.0, 0.3 )
        else if h < 0.0 then
            ( h, alpha1 seafloor, 20.0, 0.0, 0.7 )
        else if h < 0.15 then
            ( h, alpha1 sand, 80.0, 0.0, 0.7 )
        else
            ( h, alpha1 green, 0.8, 0.001, 0.3 )


sea : Float -> Maybe NoiseSurfaceVertex
sea h =
    let
        sea =
            hslToVec3 (degrees 190) 0.8 ((abs (h / 10) + 0.1) * 3)

        blue =
            hslToVec3 (degrees 196) 0.8 ((h + 0.1) * 4)

        alpha1 v0 =
            let
                v =
                    V3.toRecord v0
            in
                V4.fromTuple ( v.x, v.y, v.z, 1.0 )
    in
        if h < 0.0 then
            Just ( 0.1, alpha1 sea, 1.0, 0.7, 0.5 )
        else if h < 0.2 then
            Just ( 0.1, alpha1 blue, 1.0, 0.7, 0.5 )
        else
            Nothing



----------------------------------------------------------------------


paint : (Float -> NoiseSurfaceVertex) -> Placement -> Array2D Float -> List Body
paint how placement terrain =
    let
        paintedTerrain =
            Array2D.map how terrain
    in
        visibleTerrain placement terrain (terrainGrid 1 placement paintedTerrain)


ripplePaint : (Float -> Maybe NoiseSurfaceVertex) -> Float -> Placement -> Array2D Float -> List Body
ripplePaint how ripple placement terrain =
    let
        paintedTerrain =
            Array2D.map how terrain
    in
        visibleTerrain placement terrain (terrainGridMaybe 1 ripple placement paintedTerrain)


visibleTerrain : Placement -> Array2D Float -> Array2D (List Body) -> List Body
visibleTerrain placement terrain arr =
    let
        appears =
            Array2D.map
                (List.map (\body -> Appearance.transform (M4.translate body.position) body.appear))
                arr
    in
        List.map toBody
            [ { scale = vec3 1 1 1, position = vec3 0 0 0, orientation = Orientation.initial, appear = appearTerrain placement terrain appears } ]


appearTerrain : Placement -> Array2D Float -> Array2D (List Appearance) -> Appearance
appearTerrain placement terrain appears p =
    List.concat <|
        List.map (\appear -> appear p) <|
            nearby placement terrain p.cameraPos appears


nearby : Placement -> Array2D Float -> Vec3 -> Array2D (List Appearance) -> List Appearance
nearby placement terrain pos appears =
    let
        ix0 =
            floor ((getX pos - placement.xOffset) / (placement.xDelta * toFloat placement.tileSize))

        iz0 =
            floor ((getZ pos - placement.zOffset) / (placement.zDelta * toFloat placement.tileSize))

        getXZ x z =
            Array2D.getXY z x [] appears

        -- The visible radius of tiles depends on the height of the camera
        r =
            -- max 12 (floor ((getY pos - approxElevation placement terrain pos) / 10))
            max 20 (floor ((getY pos - approxElevation placement terrain pos) / toFloat (placement.tileSize)))

        -- r = (max 64 (floor ((getY pos - approxElevation placement terrain pos)))) // placement.tileSize
        ir =
            iradius r
    in
        List.concat (List.map (\( x, y ) -> getXZ (ix0 + x) (iz0 + y)) ir)



-- A faster version of elevation that doesn't attempt to interpolate between
-- the four surrounding points. Useful for rough calculations like deciding
-- how many nearby terrain tiles to display based on how close to the ground
-- the camera is.


approxElevation : Placement -> Array2D Float -> Vec3 -> Float
approxElevation placement terrain pos =
    let
        ix0 =
            floor <| (getX pos + 256) / 2

        iz0 =
            floor <| (getZ pos + 256) / 2

        getXZ x z =
            (Array2D.getXY x z 0 terrain) * placement.yMult
    in
        getXZ ix0 iz0


terrainGrid : Int -> Placement -> Array2D NoiseSurfaceVertex -> Array2D (List Body)
terrainGrid skip placement =
    placeTerrain (noiseSurface2D skip) placement
        << tileTerrain skip placement.tileSize


terrainGridMaybe : Int -> Float -> Placement -> Array2D (Maybe NoiseSurfaceVertex) -> Array2D (List Body)
terrainGridMaybe skip ripple placement =
    placeTerrain (rippleNoiseSurface2D skip ripple) placement
        << tileTerrain skip placement.tileSize


tileTerrain : Int -> Int -> Array2D v -> List (List ( List (List v), ( Int, Int ) ))
tileTerrain skip smallSide arr0 =
    case arr0 of
        Array2D.Array2D bigSide _ ->
            let
                coords =
                    subSquares smallSide bigSide
            in
                List.map (List.map (mkTile skip smallSide arr0)) coords



-- smallSide better be a multiple of skip


mkTile : Int -> Int -> Array2D v -> ( Int, Int ) -> ( List (List v), ( Int, Int ) )
mkTile skip smallSide arr0 ( x0, y0 ) =
    case arr0 of
        Array2D.Array2D bigSide arr ->
            let
                extent x =
                    min (x + smallSide + 1) (bigSide - 1)

                slice x y =
                    Array.toList <| Array.slice (x + y * bigSide) (extent x + y * bigSide) arr

                rows =
                    List.map (subsample skip << slice x0) (List.range y0 (extent (y0 - 1)))

                out =
                    List.reverse <| List.foldl (::) [] rows
            in
                ( out, ( x0, y0 ) )



-- placeTerrain : List (List ((List (List NoiseSurfaceVertex)), (Int, Int))) -> Array2D Body


placeTerrain : (Placement -> ( Float, Float ) -> v -> List (Oriented (Visible {}))) -> Placement -> List (List ( v, ( Int, Int ) )) -> Array2D (List Body)
placeTerrain toSurface2D placement terrainsCoords =
    let
        terrainSurfacesCoords =
            List.map (List.map (\( t, ( x, z ) ) -> ( toSurface2D placement ( toFloat x * placement.xDelta, toFloat z * placement.zDelta ) t, ( x, z ) ))) terrainsCoords

        terrainz =
            Array2D.fromLists terrainSurfacesCoords
    in
        Array2D.map
            (\( l, ( x, z ) ) ->
                (List.map (\s -> toBody
                    { s
                        | scale = vec3 1 1 1
                        , position = vec3 (toFloat x * placement.xDelta) 0 (toFloat z * placement.zDelta)
                    })
                    l
                )
            )
            terrainz
