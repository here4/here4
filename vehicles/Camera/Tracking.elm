module Camera.Tracking exposing (tracking)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Camera exposing (..)
import Ground exposing (Ground)
import Model

tracking : Ground -> Moving a -> Camera -> Camera
tracking ground target camera =
    let
        eyeLevel pos =
            Model.eyeLevel + ground.elevation pos

        -- TODO: make distance relative to target size, speed
        behind =
            sub target.position (V3.scale 17 (Model.direction target))

        newCameraPos =
            if getY behind < Model.eyeLevel then
                behind
            else
                add (vec3 0 6 0) behind

        cameraPos =
            ground.bounds newCameraPos

        cameraOrientation =
            target.orientation

    in
        { camera | position = cameraPos
                 , orientation = target.orientation}
