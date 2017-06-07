module Camera.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3)
import Orientation as Orientation

import Body exposing (..)
import Camera exposing (..)
import Model exposing (Player)


toCamera : Oriented a -> Camera
toCamera body =
    { position = body.position
    , orientation = body.orientation
    }


cameraUp : Player -> Vec3
cameraUp player =
    Orientation.rotateBodyV player.motion.orientation V3.j
