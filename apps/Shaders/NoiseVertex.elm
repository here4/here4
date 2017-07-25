module Shaders.NoiseVertex exposing (NoiseVertex, NoiseVertexInput, NoiseVertexOutput, RippleNoiseVertexInput, noiseVertex, rippleNoiseVertex)

import GLSLPasta
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import Shaders.VertexPasta exposing (..)
import Time exposing (Time)
import WebGL exposing (..)


type alias NoiseVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    , coord : Vec3
    , textureScale : Float
    , timeScale : Float
    , smoothing : Float
    }


type alias NoiseVertexInput =
    { iDetail : Float
    , iGlobalTime : Time
    , iGlobalTimeV : Time
    , iHMD : Float
    , iLensDistort : Float
    , iResolution : Vec3
    , modelViewProjectionMatrix : Mat4
    , modelMatrix : Mat4
    , viewPosition : Vec3
    , lightPosition : Vec3
    }


type alias NoiseVertexOutput =
    { elm_FragColor : Vec4
    , elm_FragCoord : Vec2
    , clipPosition : Vec4
    , iTextureScale : Float
    , iTimeScale : Float
    , iSmoothing : Float
    }


type alias RippleNoiseVertexInput =
    { iDetail : Float
    , iGlobalTime : Time
    , iGlobalTimeV : Time
    , iHMD : Float
    , iLensDistort : Float
    , iResolution : Vec3
    , iRipple : Float
    , modelViewProjectionMatrix : Mat4
    , modelMatrix : Mat4
    , viewPosition : Vec3
    , lightPosition : Vec3
    }


noiseVertex : Shader NoiseVertex NoiseVertexInput NoiseVertexOutput
noiseVertex =
    GLSLPasta.combine "noiseVertex"
        [ Lighting.vertex_gl_Position
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , Lighting.vertexNoTangent
        , vertex_noise
        , distort
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader


rippleNoiseVertex : Shader NoiseVertex RippleNoiseVertexInput NoiseVertexOutput
rippleNoiseVertex =
    GLSLPasta.combine "rippleNoiseVertex"
        [ vertex_ripple
        , Lighting.vertex_gl_Position
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , Lighting.vertexNoTangent
        , vertex_noise
        , distort
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader
