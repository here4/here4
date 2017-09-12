module Shaders.Mondrian exposing (mondrian)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.FragmentPasta exposing (..)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://github.com/hendriklammers/month-of-shaders/blob/master/src/Shader/Day3.elm
--
-- Author: Hendrik Lammers
-- License: MIT


fragment_mondrian : GLSLPasta.Component
fragment_mondrian =
    { empty
        | id = "fragment_mondrian"
        , provides = [ "diffuseColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
// rectangle that starts at bottom left
float rect(vec2 uv, vec2 pos, vec2 size) {
    vec2 p = uv - floor(uv); // Use within bounding unit square
    vec2 bl = step(pos, p);
    // Subtracting from 1.0 to invert coordinates and the top right values
    vec2 tr = step(1.0 - pos - size, vec2(1.0 - p));
    return bl.x * bl.y * tr.x * tr.y;
}

vec4 mondrian (vec2 uv) {
    // y: 0.0 to 1.0
    // x: 0.0 to xMax
    vec2 p = gl_FragCoord.xy / iResolution.y;
    // Storing xMax so it can easily be used in positional functions
    float xMax = 1.3; // iResolution.x / iResolution.y;
    vec3 cBlue = vec3(0.016, 0.016, 0.627);
    vec3 cWhite = vec3(0.984, 0.988, 0.957);
    vec3 cGray = vec3(0.102, 0.078, 0.078);
    vec3 cBlack = vec3(0.008, 0.008, 0.0);
    vec3 cRed = vec3(0.969, 0.0, 0.016);
    vec3 cYellow = vec3(0.85, 0.85, 0.0);
    vec3 c = cWhite;
    // Increment animation speed
    float speed = iGlobalTime * 0.3;
    // Use Sine wave to animate x and y positions for certain elements
    float y1 = 0.3 + sin(speed) * 0.1;
    float x1 = xMax - 0.6 + sin(speed) * 0.1;
    // Colored rectangles
    c = mix(c, cRed, rect(uv, vec2(x1, y1), vec2(xMax - x1, 1.0 - y1)));
    c = mix(c, cGray, rect(uv, vec2(0.3, y1), vec2(0.2, 0.6 - y1)));
    c = mix(c, cBlue, rect(uv, vec2(0.0, 0.0), vec2(0.3, y1)));
    c = mix(c, cYellow, rect(uv, vec2(0.3, 0.89), vec2(0.2, 0.11)));
    // Black lines
    c = mix(c, cBlack, rect(uv, vec2(0.0, 0.6), vec2(0.5, 0.03)));
    c = mix(c, cBlack, rect(uv, vec2(0.3, 0.0), vec2(0.03, 1.0)));
    c = mix(c, cBlack, rect(uv, vec2(0.5, 0.0), vec2(0.03, 1.0)));
    c = mix(c, cBlack, rect(uv, vec2(0.0, y1), vec2(xMax, 0.03)));
    c = mix(c, cBlack, rect(uv, vec2(x1, y1), vec2(0.03, 1.0 - y1)));
    c = mix(c, cBlack, rect(uv, vec2(0.3, 0.89), vec2(0.2, 0.03)));
    return vec4(c, 1.0);
}
"""
            ]
        , splices =
            [ """
            vec3 diffuseColor = mondrian(fragCoord).rgb;
"""
            ]
    }


mondrian : Shader {} { u | iResolution : Vec3, iGlobalTime : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
mondrian =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "mondrian"
        [ Lighting.fragment_lightDir
        , Lighting.fragment_interpolatedNormal
        , Lighting.fragment_lambert
        , fragment_mondrian
        , Lighting.fragment_diffuse
        , fragment_ambient
        , Lighting.fragment_specular
        , Lighting.fragment_attenuation
        , Lighting.fragment_phong
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
