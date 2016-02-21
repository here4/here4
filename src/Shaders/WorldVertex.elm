module Shaders.WorldVertex (Vertex, worldVertex) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias Vertex = { pos:Vec3, color:Vec3, coord:Vec3 }

worldVertex : Shader Vertex { u | view:Mat4 } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
worldVertex = [glsl|

attribute vec3 pos;
attribute vec3 color;
attribute vec3 coord;
uniform mat4 view;
varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;
void main () {
  gl_Position = view * vec4(pos, 1.0);
  elm_FragColor = color;
  elm_FragCoord = coord.xy;
}

|]
