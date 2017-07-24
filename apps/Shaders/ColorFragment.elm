module Shaders.ColorFragment exposing (colorFragment, noiseColorFragment)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
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
        , provides = [ "gl_FragColor" ]
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
            gl_FragColor = noise_texture(fragCoord);
"""
            ]
    }

noiseColorFragment : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float, iDetail : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4, iTextureScale : Float, iTimeScale : Float, iSmoothing : Float }
noiseColorFragment =
    GLSLPasta.combineUsingTemplate hmdTemplate
        [ fragment_noiseColor
        , Lighting.lightenDistance
        ]
    |> WebGL.unsafeShader

{-
    [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;
uniform float iHMD;
uniform float iDetail;

varying vec4 elm_FragColor;
varying vec2 elm_FragCoord;
varying vec4 clipPosition;
varying float iTextureScale;
varying float iTimeScale;
varying float iSmoothing;


const vec2 LeftLensCenter = vec2(0.2863248, 0.5);
const vec2 RightLensCenter = vec2(0.7136753, 0.5);
const vec2 LeftScreenCenter = vec2(0.25, 0.5);
const vec2 RightScreenCenter = vec2(0.75, 0.5);
const vec2 Scale = vec2(0.1469278, 0.2350845);
//const vec2 Scale = vec2(0.1469278, 0.2350845);
const vec2 ScaleIn = vec2(3, 2.5);
//const vec2 ScaleIn = vec2(2.5, 1.5);
const vec4 HmdWarpParam   = vec4(1, 0.22, 0.24, 0);

// Scales input texture coordinates for distortion.
vec2 HmdWarp(vec2 in01, vec2 LensCenter)
{
        vec2 theta = (in01 - LensCenter) * ScaleIn; // Scales to [-1, 1]
        float rSq = theta.x * theta.x + theta.y * theta.y;
        vec2 rvector = theta * (HmdWarpParam.x + HmdWarpParam.y * rSq +
                HmdWarpParam.z * rSq * rSq +
                HmdWarpParam.w * rSq * rSq * rSq);
        return LensCenter + Scale * rvector;
}


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


void texture(vec2 tc) {
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

        gl_FragColor = mix(fractalTexture, flatTexture, iSmoothing);

        float lightenDistance = clipPosition.w * 0.01;

        //gl_FragColor *= 1.0 - lightenDistance * vec4(0.07, 0.09, 0.10, 0.15);
        gl_FragColor *= 1.0 - lightenDistance * vec4(0.18, 0.21, 0.24, 0.15);
}

void hmd() {
        vec2 LensCenter = vec2(0.5, 0.5);
        vec2 ScreenCenter = vec2(0.5, 0.5);

        vec2 oTexCoord = gl_FragCoord.xy / iResolution.xy;

        vec2 tc = HmdWarp(oTexCoord, LensCenter);
        if (any(bvec2(clamp(tc,ScreenCenter-vec2(0.5,0.5), ScreenCenter+vec2(0.5,0.5)) - tc)))
        {
                gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
                return;
        }

        if (int(iDetail) == 0) {
                gl_FragColor = elm_FragColor;
        } else {
                texture(tc);
        }
}

void main() {
    if (iHMD == 1.0)
        hmd();
    else
        texture(elm_FragCoord);
}

|]
-}
