module Camera.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Orientation exposing (Orientation)

import Body exposing (..)
import Camera exposing (..)

toTarget : Moving a -> Target
toTarget thing =
    { position = thing.position
    , orientation = thing.orientation
    , velocity = thing.velocity
    }

toStationaryTarget : Oriented a -> Target
toStationaryTarget thing =
    { position = thing.position
    , orientation = thing.orientation
    , velocity = vec3 0 0 0
    }

toCamera : Oriented a -> Camera
toCamera thing =
    { position = thing.position
    , orientation = thing.orientation
    , target = toStationaryTarget thing
    }

framing : Moving a -> Framing
framing thing =
    { target =
        { position = thing.position
        , orientation = thing.orientation
        , velocity = thing.velocity
        }
    }


cameraUp : { a | orientation : Orientation } -> Vec3
cameraUp thing =
    Orientation.rotateBodyV thing.orientation V3.j


-- | Point the camera towards target
retarget : Target -> Camera -> Camera
retarget target camera =
    let
        displacement = V3.sub target.position camera.position

        orientation =
            Orientation.fromTo V3.k displacement
    in
        { camera | orientation = orientation
                 , target = target }

-- | Roll a cmaera to upright
upright : Camera -> Camera
upright camera = { camera | orientation = Orientation.upright camera.orientation }
