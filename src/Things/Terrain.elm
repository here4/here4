module Things.Terrain exposing
    ( Terrain, generate
    , bounds, elevation
    , paint, ripplePaint
    , mountains, sea
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
import Thing exposing (..)
import Things.Surface2D exposing (..)

----------------------------------------------------------------------

type alias Terrain =
    { placement : Placement
    , elevations : Array2D Float
    , groundMesh : List Thing
    , waterMesh : List Thing
    }

type Tag = TTag (Array2D Float)

generate : (Terrain -> msg) -> Placement -> Cmd msg
generate tagger placement =
    let elGen = randTerrain2D (placement.bigSide+1)
        makeTerrain elevations =
            { placement = placement
            , elevations = elevations
            , groundMesh = paint mountains placement elevations
            , waterMesh = ripplePaint sea 0.3 placement elevations
            }
    in Random.generate tagger (Random.map makeTerrain elGen)

----------------------------------------------------------------------

mountains : Float -> NoiseSurfaceVertex
mountains h =
  let green = hslToVec3
          (degrees (70 + toFloat (round ((h+0.34)*500) % 70)))
          (0.3 + h/4)
          (0.2 + (1-h)/3)
      sand = hslToVec3 (degrees 50) 0.8 ((h+0.1)*4)
      seafloor = hslToVec3 (degrees 50) 0.3 ((h+0.1)*4)
      snow = hslToVec3 (degrees 178) 0.8 h
      alpha1 v0 = let v = V3.toRecord v0 in V4.fromTuple (v.x, v.y, v.z, 1.0)
  in
      if h > 0.8 then (h, alpha1 snow, 0.8, 0.0, 0.3)
      else if h < 0.0 then (h, alpha1 seafloor, 20.0, 0.0, 0.7)
      else if h < 0.15 then (h, alpha1 sand, 80.0, 0.0, 0.7)
      else (h, alpha1 green, 0.8, 0.001, 0.3)

sea : Float -> Maybe NoiseSurfaceVertex
sea h =
  let
      sea = hslToVec3 (degrees 190) 0.8 ((abs (h/10) + 0.1)*3)
      blue = hslToVec3 (degrees 196) 0.8 ((h+0.1)*4)
      alpha1 v0 = let v = V3.toRecord v0 in V4.fromTuple (v.x, v.y, v.z, 1.0)
  in
      if h < 0.0 then Just (0.1, alpha1 sea, 1.0, 0.7, 0.5)
      else if h < 0.2 then Just (0.1, alpha1 blue, 1.0, 0.7, 0.5)
      else Nothing

----------------------------------------------------------------------

-- bounds : Placement -> Vec3 -> Vec3
bounds : Terrain -> Vec3 -> Vec3
bounds { placement } pos =
    let bound x low high = if (x < low) then low else (if x > high then high else x)
        (x,y,z) = V3.toTuple pos
    -- in vec3 (bound x -246 1782) (bound y 0 1000) (bound z -246 1782)
    in vec3
       (bound x (placement.xOffset + 10) (placement.xOffset + toFloat placement.bigSide * placement.xDelta - 10))
       (bound y 0 1000)
       (bound z (placement.zOffset + 10) (placement.zOffset + toFloat placement.bigSide * placement.zDelta - 10))

-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle
-- elevation : Placement -> Array2D Float -> Vec3 -> Float
elevation : Terrain -> Vec3 -> Float
elevation { placement, elevations } pos =
    let
        ix0 = (getX pos + 256) / 2
        ix  = floor ix0
        ixf = ix0 - toFloat ix

        iz0 = (getZ pos + 256) / 2
        iz  = floor iz0
        izf = iz0 - toFloat iz

        getXZ x z = (Array2D.getXY x z 0 elevations) * placement.yMult

        i00 = getXZ ix     iz      --     00 ... 10  -> x
        i10 = getXZ (ix+1) iz      --  |  .    /  .
                                   --  v  .   /   .
        i01 = getXZ ix     (iz+1)  --     .  /    .
        i11 = getXZ (ix+1) (iz+1)  --  z  01 ... 11

        mix a b f = (a * (1-f) + b * f) / 2 -- f describes how close to a

    in
        if ixf + izf < 1.0 then
            mix i00 i10 ixf + mix i00 i01 izf
        else
            mix i01 i11 ixf + mix i10 i11 izf

-- A faster version of elevation that doesn't attempt to interpolate between
-- the four surrounding points. Useful for rough calculations like deciding
-- how many nearby terrain tiles to display based on how close to the ground
-- the camera is.
approxElevation : Placement -> Array2D Float -> Vec3 -> Float
approxElevation placement terrain pos =
    let
        ix0 = floor <| (getX pos + 256) / 2
        iz0 = floor <| (getZ pos + 256) / 2
        getXZ x z = (Array2D.getXY x z 0 terrain) * placement.yMult
    in
        getXZ ix0 iz0
            
----------------------------------------------------------------------

paint : (Float -> NoiseSurfaceVertex) -> Placement -> Array2D Float -> List Thing
paint how placement terrain =
    let paintedTerrain = Array2D.map how terrain
    in visibleTerrain placement terrain (terrainGrid 1 placement paintedTerrain)

ripplePaint : (Float -> Maybe NoiseSurfaceVertex) -> Float -> Placement -> Array2D Float -> List Thing
ripplePaint how ripple placement terrain =
    let paintedTerrain = Array2D.map how terrain
    in visibleTerrain placement terrain (terrainGridMaybe 1 ripple placement paintedTerrain)

visibleTerrain : Placement -> Array2D Float -> Array2D Thing -> List Thing
visibleTerrain placement terrain arr =
    let
        sees = Array2D.map (\(Thing _ pos _ see) -> (tview (M4.translate pos) see)) arr
    in
        List.map extractThing
            [{ scale = vec3 1 1 1, pos = vec3 0 0 0, orientation = vec3 1 0 1, see = seeTerrain placement terrain sees }]

seeTerrain : Placement -> Array2D Float -> Array2D See -> See
seeTerrain placement terrain sees p =
       List.concat
    <| List.map (\see -> see p)
    <| nearby placement terrain p.cameraPos sees

nearby : Placement -> Array2D Float -> Vec3 -> Array2D See -> List See
nearby placement terrain pos sees =
    let
        ix0 = floor ((getX pos - placement.xOffset) / (placement.xDelta * toFloat placement.tileSize))
        iz0 = floor ((getZ pos - placement.zOffset) / (placement.zDelta * toFloat placement.tileSize))
        getXZ x z = Array2D.getXY z x (\_ -> []) sees

        -- The visible radius of tiles depends on the height of the camera
        r = max 12 (floor ((getY pos - approxElevation placement terrain pos) / 10))
        -- r = (max 64 (floor ((getY pos - approxElevation placement terrain pos)))) // placement.tileSize
        ir = iradius r
    in
        List.map (\(x,y) -> getXZ (ix0+x) (iz0+y)) ir

terrainGrid : Int -> Placement -> Array2D NoiseSurfaceVertex -> Array2D Thing
terrainGrid skip placement =
       placeTerrain (noiseSurface2D skip) placement
    << tileTerrain skip placement.tileSize

terrainGridMaybe : Int -> Float -> Placement -> Array2D (Maybe NoiseSurfaceVertex) -> Array2D Thing
terrainGridMaybe skip ripple placement =
       placeTerrain (rippleNoiseSurface2D skip ripple) placement
    << tileTerrain skip placement.tileSize

tileTerrain : Int -> Int -> Array2D v -> List (List ((List (List v), (Int, Int))))
tileTerrain skip smallSide arr0 = case arr0 of
  Array2D.Array2D bigSide _ ->
    let coords = subSquares smallSide bigSide
    in List.map (List.map (mkTile skip smallSide arr0)) coords

-- smallSide better be a multiple of skip
mkTile : Int -> Int -> Array2D v -> (Int, Int) -> (List (List v), (Int, Int))
mkTile skip smallSide arr0 (x0, y0) = case arr0 of
  Array2D.Array2D bigSide arr ->
    let extent x = min (x+smallSide+1) (bigSide - 1)
        slice x y = Array.toList <| Array.slice (x + y*bigSide) (extent x + y*bigSide) arr
        rows = List.map (subsample skip << slice x0) [y0 .. extent (y0-1)]
        out = List.reverse <| List.foldl (::) [] rows
    in (out, (x0, y0))

-- placeTerrain : List (List ((List (List NoiseSurfaceVertex)), (Int, Int))) -> Array2D Thing
placeTerrain : (Placement -> (Float, Float) -> v -> Oriented (Visible {})) -> Placement -> List (List (v, (Int, Int))) -> Array2D Thing
placeTerrain toSurface2D placement terrainsCoords =
    let
        terrainSurfacesCoords = List.map (List.map (\(t,(x,z)) -> (toSurface2D placement (toFloat x * placement.xDelta, toFloat z * placement.zDelta) t, (x,z)))) terrainsCoords
        terrainz = Array2D.fromLists terrainSurfacesCoords
    in
        Array2D.map (\(s,(x,z)) -> extractThing { s | scale = vec3 1 1 1
                                                    , pos = vec3 (toFloat x * placement.xDelta) 0 (toFloat z * placement.zDelta)
                                                }
                    ) terrainz
