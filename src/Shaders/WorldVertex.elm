module Shaders.WorldVertex exposing (Vertex, worldVertex)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias Vertex = { pos:Vec3, color:Vec3, coord:Vec3 }

worldVertex : Shader Vertex { u | iLensDistort:Float, view:Mat4 } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
worldVertex = [glsl|

attribute vec3 pos;
attribute vec3 color;
attribute vec3 coord;
uniform mat4 view;
uniform float iLensDistort;
varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;

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
  vec4 p = view * vec4(pos, 1.0);
  if (iLensDistort > 0.0) {
    gl_Position = distort(p);
  } else {
    gl_Position = p;
  }
  elm_FragColor = color;
  elm_FragCoord = coord.xy;
}

|]
