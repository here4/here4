module Shaders.TextureFragment exposing (textureFragment)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader, Texture)

textureFragment : Shader {} { u | iResolution:Vec3, iHMD:Float, iTexture:Texture } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
textureFragment = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iHMD;
uniform sampler2D iTexture;

varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;

const vec2 Scale = vec2(0.1469278, 0.2350845);
//const vec2 Scale = vec2(0.1469278, 0.2350845);
const vec2 ScaleIn = vec2(4, 2.5);
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

    gl_FragColor = texture2D(iTexture, tc);
}

void main() {
    if (iHMD == 1.0)
        hmd();
    else
        gl_FragColor = texture2D(iTexture, elm_FragCoord);
}

|]
