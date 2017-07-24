module Shaders.TextureFragment exposing (textureFragment)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (Shader, Texture)


textureCoord : GLSLPasta.Component
textureCoord =
    { empty
        | id = "textureCoord"
        , globals =
            [ Uniform "sampler2D" "iTexture"
            ]
        , splices =
            [ """
            gl_FragColor = texture2D(iTexture, fragCoord);
"""
            ]
    }

textureFragment : Shader {} { u | iResolution : Vec3, iHMD : Float, iTexture : Texture } { elm_FragColor : Vec4, elm_FragCoord : Vec2 }
textureFragment =
    GLSLPasta.combineUsingTemplate hmdTemplate [ textureCoord ]
    |> WebGL.unsafeShader
