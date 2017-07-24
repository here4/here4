module Shaders.WorldVertex exposing (Vertex, worldVertex)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting exposing (vertex_clipPosition)
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (..)
import Shaders.VertexPasta exposing (..)
import WebGL exposing (..)


type alias Vertex =
    { pos : Vec3, color : Vec4, coord : Vec3 }


{-| Forward the vertex color to the fragment shader, as vec4 elm_FragColor
-}
vertex_elm_FragColor : GLSLPasta.Component
vertex_elm_FragColor =
    { empty
        | id = "elm_FragColor"
        , globals =
            [ Attribute "vec4" "color"
            , Varying "vec4" "elm_FragColor"
            ]
        , splices =
            [ """
        elm_FragColor = color;
                """
            ]
    }


worldVertex : Shader Vertex { u | iLensDistort : Float, iPerspective : Mat4, iLookAt : Mat4 } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
worldVertex =
    GLSLPasta.combine "worldVertex"
        [ perspective
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_noise
        , distort
        , vertex_clipPosition
        ]
    |> WebGL.unsafeShader
