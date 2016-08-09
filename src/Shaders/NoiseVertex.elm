module Shaders.NoiseVertex exposing (NoiseVertex, NoiseVertexInput, NoiseVertexOutput, RippleNoiseVertexInput, noiseVertex, rippleNoiseVertex)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (..)

type alias NoiseVertex = { pos:Vec3, color:Vec4, coord:Vec3, textureScale:Float, timeScale:Float, smoothing:Float }

type alias NoiseVertexInput = -- u = { u | iGlobalTimeV:Float, iLensDistort:Float, view:Mat4 }
          { iDetail : Float
          , iGlobalTime : Time
          , iGlobalTimeV : Time
          , iHMD : Float
          , iLensDistort : Float
          , iResolution : Vec3
          , view : Mat4
          }

type alias NoiseVertexOutput = { elm_FragColor:Vec4, elm_FragCoord:Vec2, iTextureScale:Float, iTimeScale:Float, iSmoothing:Float }

type alias RippleNoiseVertexInput u = { u | iGlobalTimeV:Float, iLensDistort:Float, view:Mat4, iRipple:Float }

-- noiseVertex : Shader NoiseVertex { u | iGlobalTimeV:Float, iLensDistort:Float, view:Mat4 } { elm_FragColor:Vec4, elm_FragCoord:Vec2, iTextureScale:Float, iTimeScale:Float, iSmoothing:Float }
noiseVertex : Shader NoiseVertex (NoiseVertexInput) NoiseVertexOutput
noiseVertex = [glsl|

attribute vec3 pos;
attribute vec4 color;
attribute vec3 coord;
attribute float textureScale;
attribute float timeScale;
attribute float smoothing;
uniform float iGlobalTimeV;
uniform float iLensDistort;
uniform mat4 view;
varying vec4 elm_FragColor;
varying vec2 elm_FragCoord;
varying float iTextureScale;
varying float iTimeScale;
varying float iSmoothing;

vec4 distort(vec4 p)
{
  vec2 v = p.xy / p.w;

  // Convert to polar coords
  float theta = atan(v.y, v.x);
  float radius = length(v);

  // Distort
  radius = pow(radius, iLensDistort);

  // Convert back to Cartesian
  v.x = radius * cos(theta);
  v.y = radius * sin(theta);
  p.xy = v.xy * p.w;
  return p;
}

void main () {
  elm_FragColor = color;
  elm_FragCoord = coord.xy;
  iTextureScale = textureScale;
  iTimeScale = timeScale;
  iSmoothing = smoothing;

  vec4 p = view * vec4(pos, 1.0);
  if (iLensDistort > 0.0) {
    vec4 d = distort(p);
    gl_Position = d;
    //if (d.x < -30.0 || d.x > 30.0 || d.y < -30.0 || d.y > 30.0)
    //  elm_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  } else {
    gl_Position = p;
  }
}

|]

-- rippleNoiseVertex : Shader NoiseVertex { u | iGlobalTimeV:Float, iLensDistort:Float, view:Mat4, iRipple:Float } { elm_FragColor:Vec4, elm_FragCoord:Vec2, iTextureScale:Float, iTimeScale:Float, iSmoothing:Float }
rippleNoiseVertex : Shader NoiseVertex (RippleNoiseVertexInput a) NoiseVertexOutput
rippleNoiseVertex = [glsl|

attribute vec3 pos;
attribute vec4 color;
attribute vec3 coord;
attribute float textureScale;
attribute float timeScale;
attribute float smoothing;
uniform float iGlobalTimeV;
uniform float iLensDistort;
uniform float iRipple;
uniform mat4 view;
varying vec4 elm_FragColor;
varying vec2 elm_FragCoord;
varying float iTextureScale;
varying float iTimeScale;
varying float iSmoothing;

vec4 distort(vec4 p)
{
  vec2 v = p.xy / p.w;

  // Convert to polar coords
  float theta = atan(v.y, v.x);
  float radius = length(v);

  // Distort
  radius = pow(radius, iLensDistort);

  // Convert back to Cartesian
  v.x = radius * cos(theta);
  v.y = radius * sin(theta);
  p.xy = v.xy * p.w;
  return p;
}

void main () {
  float y = pos.y + iRipple*sin(coord.x*coord.y + iGlobalTimeV);
  vec3 newPos = vec3(pos.x, y, pos.z);

  vec4 p = view * vec4(newPos, 1.0);
  if (iLensDistort > 0.0) {
    vec4 d = distort(p);
    gl_Position = d;
    //if (d.x < -30.0 || d.x > 30.0 || d.y < -30.0 || d.y > 30.0)
    //  elm_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  } else {
    gl_Position = p;
  }

  elm_FragColor = color;
  elm_FragCoord = coord.xy;
  iTextureScale = textureScale;
  iTimeScale = timeScale;
  iSmoothing = smoothing;
}

|]
