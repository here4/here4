module Shaders.Sky exposing (sky)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/4sKGWt

fragment_sky : GLSLPasta.Component
fragment_sky =
    { empty
        | id = "fragment_sky"
        , provides =
            [ "gl_FragColor"
            ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
// License: BSD
// by Morgan McGuire, @CasualEffects

float hash(vec2 p) { return fract(1e4 * sin(17.0 * p.x + p.y * 0.1) * (0.1 + abs(sin(p.y * 13.0 + p.x)))); }

float noise(vec2 x) {
    vec2 i = floor(x), f = fract(x);

    float a = hash(i);
    float b = hash(i + vec2(1.0, 0.0));
    float c = hash(i + vec2(0.0, 1.0));
    float d = hash(i + vec2(1.0, 1.0));

    vec2 u = f * f * (3.0 - 2.0 * f);
    return mix(a, b, u.x) + (c - a) * u.y * (1.0 - u.x) + (d - b) * u.x * u.y;
}

float fbm(vec2 p) {
    const mat2 m2 = mat2(0.8, -0.6, 0.6, 0.8);

    float f = 0.5000 * noise(p); p = m2 * p * 2.02;
    f += 0.2500 * noise(p); p = m2 * p * 2.03;
    f += 0.1250 * noise(p); p = m2 * p * 2.01;
    f += 0.0625 * noise(p);
    return f / 0.9375;
}

vec3 render(vec3 light, vec3 ro, vec3 rd, float resolution) {
    vec3 col;

    // Sky with haze
    //col = vec3(0.3, 0.55, 0.8) * (1.0 - 0.8 * rd.y) * 0.9;
    col = vec3(0.09, 0.56, 1.00) * (1.0 - 0.8 * rd.y) * 0.9;

    // Sun
    float sundot = clamp(dot(rd, light), 0.0, 1.0);
    col += 0.25 * vec3(1.0, 0.7, 0.4) * pow(sundot, 8.0);
    col += 0.75 * vec3(1.0, 0.8, 0.5) * pow(sundot, 64.0);

    // Clouds
    col = mix(col, vec3(1.0, 0.95, 1.0), 0.5 *
        smoothstep(0.5, 0.8, fbm((ro.xz + rd.xz * (250000.0 - ro.y) / rd.y) * 0.000008)));

    // Horizon/atmospheric perspective
    col = mix(col, vec3(0.7, 0.75, 0.8), pow(1.0 - max(abs(rd.y), 0.0), 8.0));

    return col;
}

vec4 sky(vec2 tc) {
    const float verticalFieldOfView = 50.0 * 3.1415927 / 180.0;
    //vec3 rd = normalize(vec3(tc.xy - iResolution.xy / 2.0, iResolution.y * 0.5 / -tan(verticalFieldOfView * 0.5)));
    vec3 rd = normalize(vec3(tc.xy * 800.0, iResolution.y * 0.5 / -tan(verticalFieldOfView * 0.5)));

    vec3 light = normalize(vec3(-0.8,0.3,-0.3));

    vec3 ro = vec3(-iGlobalTime * 10000.0, sin(iGlobalTime) + 2.1 * 1000.0, 0.0);
    vec3 col = render(light, ro, rd, iResolution.y);

    // Gamma encode
    col = pow(col, vec3(0.4545));

    return vec4( col, 1.0 );
}
"""
            ]
        , splices =
            [ """
            gl_FragColor = sky(fragCoord);
"""
            ]
    }


sky : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2 }
sky =
    GLSLPasta.combineUsingTemplate hmdTemplate
        [ fragment_sky
        , Lighting.lightenDistance
        ]
    |> WebGL.unsafeShader
