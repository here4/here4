module Shaders.ColorVertex (ColorVertex, colorVertex) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias ColorVertex = { pos:Vec3, color:Vec3 }

colorVertex : Shader ColorVertex { u | view:Mat4 } { elm_FragColor:Vec3 }
colorVertex = [glsl|

attribute vec3 pos;
attribute vec3 color;
uniform mat4 view;

varying vec3 elm_FragColor;

vec4 distort(vec4 p)
{
  vec2 v = p.xy / p.w;

  // Convert to polar coords
  float theta = atan(v.y, v.x);
  float radius = length(v);

  // Distort
  radius = pow(radius, 0.85);

  // Convert back to Cartesian
  v.x = radius * cos(theta);
  v.y = radius * sin(theta);
  p.xy = v.xy * p.w;
  return p;
}

void main () {
    gl_Position = distort(view * vec4(pos, 1.0));
    vcolor = color;
}

|]
