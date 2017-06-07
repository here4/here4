module Camera exposing (..)

import Math.Vector3 as V3 exposing (Vec3)

import Orientation exposing (Orientation)

-- TODO: define cameraAdd etc.


type alias Camera =
    { position : Vec3
    , orientation : Orientation
    }
