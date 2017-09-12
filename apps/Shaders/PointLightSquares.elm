module Shaders.PointLightSquares exposing (pointLightSquares)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.FragmentPasta exposing (..)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://github.com/hendriklammers/month-of-shaders/blob/master/src/Shader/Day23.elm
--
-- Author: Hendrik Lammers
-- License: MIT


fragment_pointLightSquares : GLSLPasta.Component
fragment_pointLightSquares =
    { empty
        | id = "fragment_pointLightSquares"
        , provides = [ "diffuseColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
const float HALF_PI = 1.5707963267948966;

// Pseudo random number generator
float random (in vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898, 78.233))) * 43758.5453123);
}
float sineOut(float t) {
    return sin(t * HALF_PI);
}
vec4 pointLights(vec2 p) {
    float speed = iGlobalTime * 0.8;
    float grid = 10.0;
    vec2 uv = fract(p * grid);
    uv = abs(uv * 2.0 - 1.0);
    vec2 index = floor(p * grid);
    float t = floor(speed) + 1.0;
    // Glowing circle
    float circ = 1.0 / length(uv * 0.5) * 0.05;
    float r1 = random(index + t);
    float r2 = random(index + t + 1.0);
    // Create smooth transition between two random numbers
    float tween = mix(r1, r2, (fract(speed)));
    return vec4(vec3(circ * tween), 1.0);
}
"""
            ]
        , splices =
            [ """
            vec3 diffuseColor = pointLights(fragCoord).rgb;
"""
            ]
    }


pointLightSquares : Shader {} { u | iResolution : Vec3, iGlobalTime : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
pointLightSquares =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "pointLightSquares"
        [ Lighting.fragment_lightDir
        , Lighting.fragment_interpolatedNormal
        , Lighting.fragment_lambert
        , fragment_pointLightSquares
        , Lighting.fragment_diffuse
        , fragment_ambient
        , Lighting.fragment_specular
        , Lighting.fragment_attenuation
        , Lighting.fragment_phong
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
