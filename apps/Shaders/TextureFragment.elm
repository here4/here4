module Shaders.TextureFragment exposing (textureFragment)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Shaders.FragmentPasta exposing (..)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (Shader, Texture)


textureCoord : GLSLPasta.Component
textureCoord =
    { empty
        | id = "textureCoord"
        , provides = [ "diffuseColor" ]
        , globals =
            [ Uniform "sampler2D" "iTexture"
            ]
        , splices =
            [ """
            vec3 diffuseColor = texture2D(iTexture, fragCoord).rgb;
"""
            ]
    }


textureFragment : Shader {} { u | iResolution : Vec3, iHMD : Float, iTexture : Texture } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
textureFragment =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "textureFragment"
        [ Lighting.fragment_lightDir
        , Lighting.fragment_interpolatedNormal
        , Lighting.fragment_lambert
        , textureCoord
        , Lighting.fragment_diffuse
        , fragment_ambient
        , Lighting.fragment_specular
        , Lighting.fragment_attenuation
        , Lighting.fragment_phong
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
