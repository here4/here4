module Camera.Follow exposing (follow)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Camera exposing (..)
import Ground exposing (Ground)
import Model

follow : Ground -> Moving a -> Camera -> Camera
follow ground target camera =
    let
        eyeLevel pos =
            Model.eyeLevel + ground.elevation pos

        -- TODO: make distance relative to target size, speed
        behind =
            sub target.position (V3.scale 17 (Model.direction target))

        newCameraPos =
            add (vec3 0 6 0) behind

        cameraPos =
            ground.bounds
                (V3.add (V3.scale 0.5 newCameraPos) (V3.scale 0.5 camera.position)) -- smooth

    in
        { camera | position = cameraPos }
