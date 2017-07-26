module Shaders.ColorFragment exposing (colorFragment, noiseColorFragment)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Shaders.FragmentPasta exposing (..)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


{-| Forward the vertex color to the fragment shader, as vec4 elm_FragColor
-}
fragment_elm_FragColor : GLSLPasta.Component
fragment_elm_FragColor =
    { empty
        | id = "fragment_elm_FragColor"
        , provides =
            [ "gl_FragColor"
            ]
        , globals =
            [ Varying "vec4" "elm_FragColor"
            ]
        , splices =
            [ """
        gl_FragColor = elm_FragColor;
                """
            ]
    }


colorFragment : Shader {} u { elm_FragColor : Vec3, elm_FragCoord : Vec2 }
colorFragment =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "colorFragment"
        [ fragment_elm_FragColor
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader



-- TODO: Configure
--  * size of fractal detail (multiplier on elm_FragCoord)
--  * speed of motion (multiplier on iGlobalTime): can be motionless for solid objects
--  * swaying in the breeze (oscillate with sin)
-- TODO: make surface2D tile seamlessly


fragment_noiseColor : GLSLPasta.Component
fragment_noiseColor =
    { empty
        | id = "fragment_noiseColor"
        , provides = [ "diffuseColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Uniform "float" "iDetail"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            , Varying "float" "iTextureScale"
            , Varying "float" "iTimeScale"
            , Varying "float" "iSmoothing"
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
        int detail = int(iDetail);
        for (int i = 0; i < 7; i++) {
                if (i > detail) break;
                total += noise(n) * amplitude;
                n += n;
                amplitude *= 0.5;
        }
        return total;
}


vec4 noise_texture(vec2 tc) {
        vec3 c1 = vec3(elm_FragColor);
        vec3 c2 = c1 * vec3(0.7, 0.7, 0.7);
        vec3 c3 = c1 * vec3(0.6, 0.6, 0.6);
        vec3 c4 = c1 * vec3(0.9, 0.9, 0.9);
        vec3 c5 = vec3(0.10);
        vec3 c6 = vec3(0.50);

        //vec2 p = elm_FragCoord.xy * iTextureScale;
        vec2 p = tc.xy * iTextureScale;

        float scaledTime = iGlobalTime * iTimeScale;
        float q;
        vec2 r;
        vec3 c;

        int detail = int(iDetail);

        if (detail > 5) {
                q = fbm(p - scaledTime * 0.1);
                r = vec2(fbm(p + q + scaledTime * 0.7 - p.x - p.y), fbm(p + q - scaledTime * 0.4));
                c = mix(c1, c2, fbm(p + r)) + mix(c3, c4, r.x) - mix(c5, c6, r.y);
        } else if (detail > 2) {
                q = fbm(p - scaledTime * 0.1);
                r = vec2(fbm(p + q + scaledTime * 0.7 - p.x - p.y), fbm(p + q - scaledTime * 0.4));
                c = mix(c1, c2, fbm(p + r));
        } else {
                q = fbm(p);
                c = mix(c1, c2, q);
        }

        vec4 fractalTexture = vec4(c * cos(1.57 * gl_FragCoord.y / iResolution.y), elm_FragColor.a);
        vec4 flatTexture = elm_FragColor;

        return mix(fractalTexture, flatTexture, iSmoothing);
}

"""
            ]
        , splices =
            [ """
            vec3 diffuseColor = noise_texture(fragCoord).rgb;
"""
            ]
    }


-- noiseColorFragment : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float, iDetail : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4, iTextureScale : Float, iTimeScale : Float, iSmoothing : Float }
noiseColorFragment =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "noiseColorFragment"
        [ Lighting.fragment_lightDir
        , Lighting.fragment_interpolatedNormal
        , Lighting.fragment_lambert
        , fragment_noiseColor
        , Lighting.fragment_diffuse
        , fragment_ambient_07
        , Lighting.fragment_specular
        , Lighting.fragment_attenuation
        , Lighting.fragment_phong
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
