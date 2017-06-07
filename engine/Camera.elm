module Camera exposing (..)

import Math.Vector3 as V3 exposing (Vec3)

-- TODO: define cameraAdd etc.


type alias Camera =
    { position : Vec3
    , orientation : Vec3
    }
