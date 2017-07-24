module Shaders.ColorVertex exposing
    ( ColorVertex, colorVertex)


import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (..)


type alias ColorVertex =
    { pos : Vec3, color : Vec4 }


colorVertex : Shader ColorVertex { u | iLensDistort : Float, iPerspective : Mat4, iLookAt : Mat4 } { elm_FragColor : Vec4 }
colorVertex =
    GLSLPasta.combine "colorVertex"
        [ perspective
        , distort
        , vertex_elm_FragColor
        ]
    |> WebGL.unsafeShader

