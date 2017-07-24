module Shaders.NoiseVertex exposing (NoiseVertex, NoiseVertexInput, NoiseVertexOutput, RippleNoiseVertexInput, noiseVertex, rippleNoiseVertex)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting exposing (vertex_clipPosition)
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import Shaders.VertexPasta exposing (..)
import Time exposing (Time)
import WebGL exposing (..)


type alias NoiseVertex =
    { pos : Vec3, color : Vec4, coord : Vec3, textureScale : Float, timeScale : Float, smoothing : Float }


type alias NoiseVertexInput =
    { iDetail : Float
    , iGlobalTime : Time
    , iGlobalTimeV : Time
    , iHMD : Float
    , iLensDistort : Float
    , iResolution : Vec3
    , iPerspective : Mat4
    , iLookAt : Mat4
    }


type alias NoiseVertexOutput =
    { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4, iTextureScale : Float, iTimeScale : Float, iSmoothing : Float }


type alias RippleNoiseVertexInput =
    { iDetail : Float
    , iGlobalTime : Time
    , iGlobalTimeV : Time
    , iHMD : Float
    , iLensDistort : Float
    , iResolution : Vec3
    , iRipple : Float
    , iPerspective : Mat4
    , iLookAt : Mat4
    }


noiseVertex : Shader NoiseVertex NoiseVertexInput NoiseVertexOutput
noiseVertex =
    GLSLPasta.combine "noiseVertex"
        [ perspective
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_noise
        , distort
        , vertex_clipPosition
        ]
    |> WebGL.unsafeShader


rippleNoiseVertex : Shader NoiseVertex RippleNoiseVertexInput NoiseVertexOutput
rippleNoiseVertex =
    GLSLPasta.combine "rippleNoiseVertex"
        [ vertex_ripple
        , perspective
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_noise
        , distort
        , vertex_clipPosition
        ]
    |> WebGL.unsafeShader
