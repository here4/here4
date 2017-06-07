module Camera.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3)
import Orientation as Orientation

import Body exposing (..)
import Camera exposing (..)
import Model exposing (Player)

bodyCamera : Oriented a -> Camera
bodyCamera body =
    { position = body.position
    , orientation = Orientation.rotateBodyV body.orientation V3.k
    }

cameraUp : Player -> Vec3
cameraUp player =
    Orientation.rotateBodyV player.motion.orientation (V3.negate V3.j)
