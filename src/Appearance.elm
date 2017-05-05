module Appearance exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity)
import Window

type alias Perception = {
    cameraPos   : Vec3,
    windowSize  : Window.Size,
    globalTime  : Time,
    viewMatrix  : Mat4,
    lensDistort : Float,
    cameraVR    : Bool,
    measuredFPS : Float
}

type alias Appearance = Perception -> List Entity

type alias ShaderPerception =
   { iGlobalTime : Time
   , iHMD : Float
   , iResolution : Vec3
   , iLensDistort : Float
   , view : Mat4
   }
