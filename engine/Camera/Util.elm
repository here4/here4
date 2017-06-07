module Camera.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3)
import Orientation exposing (Orientation)

import Body exposing (..)
import Camera exposing (..)
import Model exposing (Player)


toCamera : Oriented a -> Camera
toCamera thing =
    { position = thing.position
    , orientation = thing.orientation
    }


cameraUp : { a | orientation : Orientation } -> Vec3
cameraUp thing =
    Orientation.rotateBodyV thing.orientation V3.j
