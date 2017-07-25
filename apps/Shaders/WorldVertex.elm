module Shaders.WorldVertex exposing (Vertex, worldVertex)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (..)
import Shaders.VertexPasta exposing (..)
import WebGL exposing (..)


type alias Vertex =
    { position : Vec3
    , color : Vec4
    , coord : Vec3
    }


worldVertex : Shader Vertex { u | iLensDistort : Float, modelViewProjectionMatrix : Mat4 } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
worldVertex =
    GLSLPasta.combine "worldVertex"
        [ Lighting.vertex_gl_Position
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_noise
        , distort
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader
