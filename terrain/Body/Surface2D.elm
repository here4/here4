module Body.Surface2D
    exposing
        ( SurfaceVertex
        , surface2D
        , NoiseSurfaceVertex
        , noiseSurface2D
        , rippleNoiseSurface2D
        )

import List exposing (..)
import Math.Vector3 as V3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 as M4
import Util exposing (subsample)
import WebGL exposing (..)
import Appearance exposing (..)
import Body exposing (Oriented, Visible)
import Orientation
import Placement exposing (..)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)


-- import Model


type alias SurfaceVertex =
    ( Float, Vec4 )



-- (height, color, textureScale, timeScale, smoothing)


type alias NoiseSurfaceVertex =
    ( Float, Vec4, Float, Float, Float )


toNSV : ( Float, Vec4 ) -> NoiseSurfaceVertex
toNSV ( y, rgb ) =
    ( y, rgb, 0.0, 0.0, 0.0 )


surface2D :
    Int
    -> Placement
    -> ( Float, Float )
    -> List (List SurfaceVertex)
    -> Oriented (Visible {})
surface2D skip placement xz =
    surface noiseVertex noiseColorFragment
        << surfaceMesh xz skip placement
        << List.map (List.map toNSV)


noiseSurface2D :
    Int
    -> Placement
    -> ( Float, Float )
    -> List (List NoiseSurfaceVertex)
    -> Oriented (Visible {})
noiseSurface2D skip placement xz =
    surface noiseVertex noiseColorFragment
        << surfaceMesh xz skip placement


surface :
    Shader NoiseVertex NoiseVertexInput b
    -> Shader {} NoiseVertexInput b
    -> Mesh NoiseVertex
    -> Oriented (Visible {})
surface vertexShader fragmentShader mesh =
    let
        appear =
            appearSurface vertexShader fragmentShader mesh
    in
        { scale = vec3 1 1 1
        , position = vec3 0 0 0
        , orientation = Orientation.initial
        , appear = appear
        }


appearSurface :
    Shader NoiseVertex NoiseVertexInput b
    -> Shader {} NoiseVertexInput b
    -> Mesh NoiseVertex
    -> Appearance
appearSurface vertexShader fragmentShader mesh p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        detail =
            p.measuredFPS / 3.0
    in
        [ entity vertexShader
            fragmentShader
            mesh
            { iGlobalTime = s
            , iResolution = resolution
            , iHMD = iHMD
            , iDetail = detail
            , iGlobalTimeV = s
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            }
        ]


rippleNoiseSurface2D :
    Int
    -> Float
    -> Placement
    -> ( Float, Float )
    -> List (List (Maybe NoiseSurfaceVertex))
    -> Oriented (Visible {})
rippleNoiseSurface2D skip ripple placement xz =
    rippleSurface rippleNoiseVertex noiseColorFragment ripple
        << surfaceMeshMaybe xz skip placement


rippleSurface :
    Shader NoiseVertex RippleNoiseVertexInput b
    -> Shader {} RippleNoiseVertexInput b
    -> Float
    -> Mesh NoiseVertex
    -> Oriented (Visible {})
rippleSurface vertexShader fragmentShader ripple mesh =
    let
        appear =
            rippleAppearSurface vertexShader fragmentShader ripple mesh
    in
        { scale = vec3 1 1 1
        , position = vec3 0 0 0
        , orientation = Orientation.initial
        , appear = appear
        }


rippleAppearSurface :
    Shader NoiseVertex RippleNoiseVertexInput b
    -> Shader {} RippleNoiseVertexInput b
    -> Float
    -> Mesh NoiseVertex
    -> Appearance
rippleAppearSurface vertexShader fragmentShader ripple mesh p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        detail =
            p.measuredFPS / 3.0
    in
        [ entity vertexShader
            fragmentShader
            mesh
            { iGlobalTime = s
            , iResolution = resolution
            , iHMD = iHMD
            , iDetail = detail
            , iGlobalTimeV = s
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , iRipple = ripple
            }
        ]



----------------------------------------------------------------------

type alias VertexLike v =
    { v | position : Vec3
        , normal : Vec3
    }

-- Make a triangle facet, with normals at all vertices pointing in the
-- same direction
makeFacet :
    VertexLike v -> VertexLike v -> VertexLike v
    -> ( VertexLike v, VertexLike v, VertexLike v )
makeFacet v1 v2 v3 =
    let
        p1 = v1.position
        p2 = v2.position
        p3 = v3.position
        normal = V3.cross (V3.sub p2 p1) (V3.sub p3 p1)

        setNormal v =
            { v | normal = normal }
    in
        ( setNormal v1, setNormal v2, setNormal v3)

mkStrip :
    List (VertexLike v) -> List (VertexLike v)
    -> List ( VertexLike v, VertexLike v, VertexLike v )
mkStrip vs1 vs2 =
    map3 makeFacet vs1 vs2 (drop 1 vs1) ++ map3 makeFacet vs2 (drop 1 vs1) (drop 1 vs2)


matRow : ( Float, Float ) -> Int -> Placement -> Float -> List NoiseSurfaceVertex -> List NoiseVertex
matRow ( rx, rz ) skip placement z =
    let
        m posOffset coordOffset ys0 =
            case ys0 of
                ( y, rgb, tex, tim, smoo ) :: ys ->
                    ({ position =
                        vec3 (placement.xOffset + posOffset)
                            (placement.yOffset + y * placement.yMult)
                            z
                     , normal = vec3 0 1 0 -- placeholder
                     , color = rgb
                     , coord = vec3 coordOffset (rz + z) 0
                     , textureScale = tex
                     , timeScale = tim
                     , smoothing = smoo
                     }
                    )
                        :: (m (posOffset + toFloat skip * placement.xDelta)
                                (coordOffset + toFloat skip * placement.xDelta)
                                ys
                           )

                _ ->
                    []
    in
        m 0.0 rx


surfaceMesh :
    ( Float, Float )
    -> Int
    -> Placement
    -> List (List NoiseSurfaceVertex)
    -> Mesh NoiseVertex
surfaceMesh ( rx, rz ) skip placement m =
    let
        zs =
            indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m

        rows =
            List.map2 (matRow ( rx, rz ) skip placement) (subsample skip zs) (subsample skip m)
    in
        triangles <| List.concat <| List.map2 mkStrip rows (drop 1 rows)



----------------------------------------------------------------------


mkStripMaybe : List (Maybe (VertexLike v)) -> List (Maybe (VertexLike v))
    -> List ( VertexLike v, VertexLike v, VertexLike v )
mkStripMaybe vs1 vs2 =
    let
        mkMaybe triangle =
            case triangle of
                ( Just v1, Just v2, Just v3 ) ->
                    Just ( makeFacet v1 v2 v3 )

                _ ->
                    Nothing

        strip =
            map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)
    in
        List.filterMap mkMaybe strip


matRowMaybe : ( Float, Float ) -> Int -> Placement -> Float -> List (Maybe NoiseSurfaceVertex) -> List (Maybe NoiseVertex)
matRowMaybe ( rx, rz ) skip placement z =
    let
        m posOffset coordOffset ys0 =
            case ys0 of
                (Just ( y, rgb, tex, tim, smoo )) :: ys ->
                    (Just
                        { position =
                            vec3 (placement.xOffset + posOffset)
                                (placement.yOffset + y * placement.yMult)
                                z
                        , normal = vec3 0 1 0 -- placeholder
                        , color = rgb
                        , coord = vec3 coordOffset (rz + z) 0
                        , textureScale = tex
                        , timeScale = tim
                        , smoothing = smoo
                        }
                    )
                        :: (m (posOffset + toFloat skip * placement.xDelta)
                                (coordOffset + toFloat skip * placement.xDelta)
                                ys
                           )

                Nothing :: ys ->
                    Nothing :: (m (posOffset + toFloat skip * placement.xDelta) (coordOffset + toFloat skip * placement.xDelta) ys)

                _ ->
                    []
    in
        m 0.0 rx


surfaceMeshMaybe :
    ( Float, Float )
    -> Int
    -> Placement
    -> List (List (Maybe NoiseSurfaceVertex))
    -> Mesh NoiseVertex
surfaceMeshMaybe ( rx, rz ) skip placement m =
    let
        zs =
            indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m

        rows =
            List.map2 (matRowMaybe ( rx, rz ) skip placement) (subsample skip zs) (subsample skip m)
    in
        triangles <| List.concat <| List.map2 mkStripMaybe rows (drop 1 rows)
