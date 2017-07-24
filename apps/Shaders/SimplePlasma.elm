module Shaders.SimplePlasma exposing (simplePlasma)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/ldBGRR


fragment_simplePlasma : GLSLPasta.Component
fragment_simplePlasma =
    { empty
        | id = "fragment_simplePlasma"
        , provides = [ "gl_FragColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
vec4 plasma(vec2 uv)
{
    vec2 p = -1.0 + 2.0 * uv;

    // main code, *original shader by: 'Plasma' by Viktor Korsun (2011)
    float x = p.x;
    float y = p.y;
    float mov0 = x+y+cos(sin(iGlobalTime)*2.0)*100.+sin(x/100.)*1000.;
    float mov1 = y / 0.9 +  iGlobalTime;
    float mov2 = x / 0.2;
    float c1 = abs(sin(mov1+iGlobalTime)/2.+mov2/2.-mov1-mov2+iGlobalTime);
    float c2 = abs(sin(c1+sin(mov0/1000.+iGlobalTime)+sin(y/40.+iGlobalTime)+sin((x+y)/100.)*3.));
    float c3 = abs(sin(c2+cos(mov1+mov2+c2)+cos(mov2)+sin(x/1000.)));
    return vec4(c1,c2,c3,1);
}
"""
            ]
        , splices =
            [ """
            gl_FragColor = plasma(fragCoord);
"""
            ]
    }


simplePlasma : Shader {} { u | iResolution : Vec3, iGlobalTime : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
simplePlasma =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "simplePlasma"
        [ fragment_simplePlasma
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
