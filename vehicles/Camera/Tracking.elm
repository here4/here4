module Camera.Tracking exposing (tracking)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Camera exposing (..)
import Camera.Util as Camera
import Ground exposing (Ground)
import Model

tracking : Shot
tracking =
    { label = "Tracking"
    , init = trackingInit
    , shoot = trackingShoot
    }

trackingInit : Ground -> Camera -> Camera
trackingInit ground camera =
    let
        target = camera.target
        position =
            sub target.position (V3.scale 17 (Model.direction target))
    in Camera.retarget camera.target { camera | position = position }

trackingShoot : Ground -> Input -> Moving a -> Camera -> Camera
trackingShoot ground input target camera =
    let
        eyeLevel pos =
            Model.eyeLevel + ground.elevation pos

        -- input
        inputNearFar =
            -input.y * 30 * input.dt

        above = vec3 0 6 0

        originalAbove = Orientation.rotateBodyV camera.target.orientation above
        newAbove = Orientation.rotateBodyV target.orientation above

        originalCameraBehind =
            if getY camera.target.position < Model.eyeLevel then
                camera.position
            else
                V3.sub camera.position originalAbove

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub originalCameraBehind camera.target.position

        originalDistance =
            V3.length originalDisplacement

        newDistance =
            clamp 10 100 (originalDistance + inputNearFar)

        -- TODO: make distance relative to target size, speed
        behind =
            sub target.position (V3.scale newDistance (Model.direction target))

        newCameraPos =
            if getY target.position < Model.eyeLevel then
                behind
            else
                add behind newAbove

        cameraPos =
            ground.bounds newCameraPos

        cameraOrientation =
            target.orientation

    in
        { camera | position = cameraPos
                 , orientation = target.orientation
                 , target = Camera.toTarget target
                 }
