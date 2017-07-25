module Appearance exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity)
import Window


type alias Perception =
    { cameraPos : Vec3
    , windowSize : Window.Size
    , globalTime : Time
    , perspective : Mat4 -- p.viewMatrix = M4.mul p.perspective p.lookAt
    , lookAt : Mat4
    , lensDistort : Float
    , cameraVR : Bool
    , measuredFPS : Float
    }


type alias Appearance =
    Perception -> List Entity


type alias ShaderPerception =
    { iGlobalTime : Time
    , iHMD : Float
    , iResolution : Vec3
    , iLensDistort : Float
    , modelViewProjectionMatrix : Mat4
    , modelMatrix : Mat4
    , viewPosition : Vec3
    }



-- | Transform the lookAt of an Appearance


transform : (Mat4 -> Mat4) -> Appearance -> Appearance
transform f appear p =
    appear { p | lookAt = f p.lookAt }
