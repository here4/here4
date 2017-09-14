module Body.Surface2D
    exposing
        ( SurfaceVertex
        , surface2D
        , NoiseSurfaceVertex
        , noiseSurface2D
        , rippleNoiseSurface2D
        )

import Geometry.VertexStrip exposing (..)
import Here4.Appearance exposing (..)
import Here4.Body exposing (Oriented, Visible)
import Here4.Orientation as Orientation
import Here4.Placement exposing (..)
import List exposing (..)
import Math.Vector3 as V3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 as M4
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)
import Util exposing (subsample)
import WebGL exposing (..)


type alias SurfaceVertex =
    ( Float, Vec4 )


type alias NoiseSurfaceVertex =
    { height : Float
    , color : Vec4
    , textureScale : Float
    , timeScale : Float
    , smoothing : Float
    }


toNSV : ( Float, Vec4 ) -> NoiseSurfaceVertex
toNSV ( y, rgb ) =
    { height = y
    , color = rgb
    , textureScale = 0.0
    , timeScale = 0.0
    , smoothing = 0.0
    }


surface2D :
    Int
    -> Placement
    -> ( Float, Float )
    -> List (List SurfaceVertex)
    -> List (Oriented (Visible {}))
surface2D skip placement xz =
    List.map (surface noiseVertex noiseColorFragment)
        << surfaceMesh xz skip placement
        << List.map (List.map toNSV)


noiseSurface2D :
    Int
    -> Placement
    -> ( Float, Float )
    -> List (List NoiseSurfaceVertex)
    -> List (Oriented (Visible {}))
noiseSurface2D skip placement xz =
    List.map (surface noiseVertex noiseColorFragment)
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
            , ambientColor = p.ambientColor
            , lightPosition = p.lightPosition
            }
        ]


rippleNoiseSurface2D :
    Int
    -> Float
    -> Placement
    -> ( Float, Float )
    -> List (List (Maybe NoiseSurfaceVertex))
    -> List (Oriented (Visible {}))
rippleNoiseSurface2D skip ripple placement xz =
    List.map (rippleSurface rippleNoiseVertex noiseColorFragment ripple)
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
            , ambientColor = p.ambientColor
            , lightPosition = p.lightPosition
            , iRipple = ripple
            }
        ]



----------------------------------------------------------------------


matRow : ( Float, Float ) -> Int -> Placement -> Float -> List NoiseSurfaceVertex -> List NoiseVertex
matRow ( rx, rz ) skip placement z =
    let
        m posOffset coordOffset ys0 =
            case ys0 of
                -- ( y, rgb, tex, tim, smoo ) :: rest ->
                nsv :: rest ->
                    ({ position =
                        vec3 (placement.xOffset + posOffset)
                            (placement.yOffset + nsv.height * placement.yMult)
                            z
                     , normal = vec3 0 1 0 -- placeholder
                     , color = nsv.color
                     , coord = vec3 coordOffset (rz + z) 0
                     , textureScale = nsv.textureScale
                     , timeScale = nsv.timeScale
                     , smoothing = nsv.smoothing
                     }
                    )
                        :: (m (posOffset + toFloat skip * placement.xDelta)
                                (coordOffset + toFloat skip * placement.xDelta)
                                rest
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
    -> List (Mesh NoiseVertex)
surfaceMesh ( rx, rz ) skip placement m =
    let
        zs =
            indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m

        rows =
            List.map2 (matRow ( rx, rz ) skip placement) (subsample skip zs) (subsample skip m)
    in
        List.map triangleStrip <| List.map2 mkStrip rows (drop 1 rows)


----------------------------------------------------------------------


matRowMaybe : ( Float, Float ) -> Int -> Placement -> Float -> List (Maybe NoiseSurfaceVertex) -> List (Maybe NoiseVertex)
matRowMaybe ( rx, rz ) skip placement z =
    let
        m posOffset coordOffset ys0 =
            case ys0 of
                -- (Just ( y, rgb, tex, tim, smoo )) :: rest ->
                (Just nsv) :: rest ->
                    (Just
                        { position =
                            vec3 (placement.xOffset + posOffset)
                                (placement.yOffset + nsv.height * placement.yMult)
                                z
                        , normal = vec3 0 1 0 -- placeholder
                        , color = nsv.color
                        , coord = vec3 coordOffset (rz + z) 0
                        , textureScale = nsv.textureScale
                        , timeScale = nsv.timeScale
                        , smoothing = nsv.smoothing
                        }
                    )
                        :: (m (posOffset + toFloat skip * placement.xDelta)
                                (coordOffset + toFloat skip * placement.xDelta)
                                rest
                           )

                Nothing :: rest ->
                    Nothing :: (m (posOffset + toFloat skip * placement.xDelta) (coordOffset + toFloat skip * placement.xDelta) rest)

                _ ->
                    []
    in
        m 0.0 rx


surfaceMeshMaybe :
    ( Float, Float )
    -> Int
    -> Placement
    -> List (List (Maybe NoiseSurfaceVertex))
    -> List (Mesh NoiseVertex)
surfaceMeshMaybe ( rx, rz ) skip placement m =
    let
        zs =
            indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m

        rows =
            List.map2 (matRowMaybe ( rx, rz ) skip placement) (subsample skip zs) (subsample skip m)
    in
        [ triangles <| List.concat <| List.map2 mkStripMaybe rows (drop 1 rows) ]
