module Here4.Camera exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Here4.Orientation as Orientation exposing (Orientation)
import Here4.Smooth as Smooth
import Here4.Body exposing (..)
import Here4.Camera.Types exposing (..)


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
    , fovy = 45
    }


toFraming : Moving a -> Framing
toFraming thing =
    let
        target =
            toTarget thing

        ahead =
            V3.add thing.position <|
                V3.scale 10.0 <|
                    Orientation.rotateBodyV thing.orientation V3.k
    in
        { target = target
        , pov =
            { position = thing.position
            , orientation = thing.orientation
            , target = { target | position = ahead }
            , fovy = 45
            }
        }


defaultCamera : Camera
defaultCamera =
    toCamera
        { position = vec3 0 1.8 0
        , orientation = Orientation.initial
        }


cameraUp : { a | orientation : Orientation } -> Vec3
cameraUp thing =
    Orientation.rotateBodyV thing.orientation V3.j



-- | Point the camera towards target


retarget : Target -> Camera -> Camera
retarget target camera =
    let
        displacement =
            V3.sub target.position camera.position

        orientation =
            Orientation.fromTo V3.k displacement
    in
        { camera
            | orientation = orientation
            , target = target
        }



-- | Roll a cmaera to upright


rollUpright : Camera -> Camera
rollUpright camera =
    { camera | orientation = Orientation.rollUpright camera.orientation }


interpolate : Float -> Camera -> Camera -> Camera
interpolate alpha oldCamera newCamera =
    { position =
        V3.add
            (V3.scale (1.0 - alpha) oldCamera.position)
            (V3.scale alpha newCamera.position)
    , orientation =
        newCamera.orientation
    , target =
        newCamera.target
    , fovy =
        ((1.0 - alpha) * oldCamera.fovy) + (alpha * newCamera.fovy)
    }



-- | Given a list of coefficients, previous raw cameras and a new raw camera,
-- return a new smoothed camera
--
-- Assume the input cameras is already reversed, such
-- that it can be built by prepending new elements


smooth : List Float -> List Camera -> Camera
smooth coeffs cameras =
    let
        sum newCamera oldCamera =
            { position = V3.add newCamera.position oldCamera.position
            , orientation = newCamera.orientation
            , target = newCamera.target
            , fovy = newCamera.fovy + oldCamera.fovy
            }

        scale f camera =
            { position = V3.scale f camera.position
            , orientation = camera.orientation
            , target = camera.target
            , fovy = f * camera.fovy
            }

        pad =
            Maybe.withDefault defaultCamera (List.head cameras)
    in
        Smooth.smooth sum scale pad coeffs cameras
