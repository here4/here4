module Camera.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3)
import Orientation as Orientation

import Model exposing (Player)

cameraUp : Player -> Vec3
cameraUp player =
    Orientation.rotateBodyV player.motion.orientation (V3.negate V3.j)
