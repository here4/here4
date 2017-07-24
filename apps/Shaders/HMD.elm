module Shaders.HMD exposing (hmdTemplate)

import GLSLPasta
import GLSLPasta.Types as GLSLPasta exposing (Global(..))

hmdTemplate : GLSLPasta.Template
hmdTemplate =
    { id = "hmdTemplate"
    , globals =
        [ Uniform "vec3" "iResolution"
        , Uniform "float" "iHMD"
        , Varying "vec4" "elm_FragColor"
        , Varying "vec2" "elm_FragCoord"
        , Const "vec2" "Scale" "vec2(0.1469278, 0.2350845)"
        -- , Const "vec2" "Scale" "vec2(0.1469278, 0.2350845)"
        , Const "vec2" "ScaleIn" "vec2(3, 2.5)"
        -- , Const "vec2" "ScaleIn" "vec2(2.5, 1.5)"
        , Const "vec4" "HmdWarpParam" "vec4(1, 0.22, 0.24, 0)"
        ]
    , template = """
precision mediump float;

__PASTA_GLOBALS__

__PASTA_FUNCTIONS__

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

void paintFragCoord(vec2 fragCoord)
{
    __PASTA_SPLICES__
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

    paintFragCoord(tc);
}

void main() {
    if (iHMD == 1.0)
        hmd();
    else
        paintFragCoord(elm_FragCoord);
}
"""
    }

