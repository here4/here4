module Camera.POV exposing (pov)

import Body exposing (Moving)
import Camera exposing (..)
import Camera.Util exposing (toCamera)
import Ground exposing (Ground)

pov : Shot
pov =
    { label = "POV"
    , init = povInit
    , shoot = povShoot
    }

povInit : Ground -> Camera -> Camera
povInit ground camera =
    { camera | position = camera.target.position }

povShoot : Ground -> Input -> Moving a -> Camera -> Camera
povShoot ground input target camera = toCamera target
