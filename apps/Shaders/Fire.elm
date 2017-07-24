module Shaders.Fire exposing (fire)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/Xsl3zN

fragment_fire : GLSLPasta.Component
fragment_fire =
    { empty
        | id = "fragment_fire"
        , provides =
            [ "gl_FragColor"
            ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Uniform "float" "iDetail"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
// by @301z

float rand(vec2 n) {
        return fract(sin(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
}

float noise(vec2 n) {
        const vec2 d = vec2(0.0, 1.0);
        vec2 b = floor(n), f = smoothstep(vec2(0.0), vec2(1.0), fract(n));
        return mix(mix(rand(b), rand(b + d.yx), f.x), mix(rand(b + d.xy), rand(b + d.yy), f.x), f.y);
}

float fbm(vec2 n) {
        float total = 0.0, amplitude = 1.0;
        for (int i = 0; i < 7; i++) {
                total += noise(n) * amplitude;
                n += n;
                amplitude *= 0.5;
        }
        return total;
}

vec4 fire(vec2 tc)
{
        const vec3 c1 = vec3(0.1, 0.0, 0.0);
        const vec3 c2 = vec3(0.7, 0.0, 0.0);
        const vec3 c3 = vec3(0.2, 0.0, 0.0);
        const vec3 c4 = vec3(1.0, 0.9, 0.0);
        const vec3 c5 = vec3(0.1);
        const vec3 c6 = vec3(0.9);
        //vec2 p = gl_FragCoord.xy * 8.0 / iResolution.xx;
        vec2 p = tc.xy * 8.0;
        float q = fbm(p - iGlobalTime * 0.1);
        vec2 r = vec2(fbm(p + q + iGlobalTime * 0.7 - p.x - p.y), fbm(p + q - iGlobalTime * 0.4));
        vec3 c = mix(c1, c2, fbm(p + r)) + mix(c3, c4, r.x) - mix(c5, c6, r.y);
        return vec4(c * cos(1.57 * gl_FragCoord.y / iResolution.y), 1.0);
}
"""
            ]
        , splices =
            [ """
        gl_FragColor = fire(fragCoord);
                """
            ]
    }

fire : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2 }
fire =
    GLSLPasta.combineUsingTemplate hmdTemplate
        [ fragment_fire
        , Lighting.lightenDistance
        ]
    |> WebGL.unsafeShader
