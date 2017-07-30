module Here4.Camera.POV exposing (pov)

import Here4.Body exposing (Moving)
import Here4.Camera.Types exposing (..)
import Here4.Ground exposing (Ground)


pov : Shot
pov =
    { label = "POV"
    , init = povInit
    , shoot = povShoot
    }


povInit : Ground -> Camera -> Camera
povInit ground camera =
    { camera | position = camera.target.position }


povShoot : Ground -> Input -> Framing -> Camera -> Camera
povShoot ground input framing camera =
    framing.pov
