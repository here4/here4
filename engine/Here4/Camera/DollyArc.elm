module Here4.Camera.DollyArc exposing (dollyZoom, dolly, zoom, arc)

import Math.Vector3 as V3 exposing (..)
import Here4.Orientation as Orientation
import Here4.Body exposing (Moving)
import Here4.Camera.Types exposing (..)
import Here4.Camera as Camera
import Here4.Ground exposing (Ground)
import Here4.Model as Model


dollyZoom : Shot
dollyZoom =
    { label = "Dolly Zoom"
    , init = dollyInit
    , shoot = dollyZoomShoot 3.0
    }


dolly : Shot
dolly =
    { label = "Dolly"
    , init = dollyInit
    , shoot = dollyShoot 3.0
    }


zoom : Shot
zoom =
    { label = "Zoom"
    , init = dollyInit
    , shoot = zoomShoot
    }


arc : Shot
arc =
    { label = "Arc"
    , init = dollyInit
    , shoot = arcShoot
    }


dollyInit : Ground -> Camera -> Camera
dollyInit ground camera =
    let
        target =
            camera.target

        position =
            sub target.position (V3.scale 17 (Model.direction target))
    in
        { camera | position = ground.bounds 0.1 position }
            |> Camera.retarget camera.target
            |> Camera.rollUpright


dollyShoot : Float -> Ground -> Input -> Framing -> Camera -> Camera
dollyShoot minDistance ground input framing camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        -- input
        inputNearFar =
            -input.y * 30 * input.dt

        inputYaw =
            -input.x * 1 * input.dt

        target =
            framing.target

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        -- New position of the camera, just tracking the target
        trackingPosition =
            V3.add target.position originalDisplacement

        -- Vector to move closer to target
        moveCloser =
            V3.scale inputNearFar (V3.normalize originalDisplacement)

        -- Displacement looking at target from newPosition
        closeDisplacement =
            V3.add originalDisplacement moveCloser

        newDisplacement =
            moveAroundSphere 0 inputYaw closeDisplacement

        -- The new position, relative to where the target is now
        newPosition =
            V3.add target.position newDisplacement

        -- Limit how close you can get. This should be a function of the size of the thing.
        position =
            if V3.length newDisplacement < minDistance then
                trackingPosition
            else
                newPosition
    in
        { camera | position = ground.bounds 0.1 position }
            |> Camera.retarget target
            |> Camera.rollUpright


dollyZoomShoot : Float -> Ground -> Input -> Framing -> Camera -> Camera
dollyZoomShoot minDistance ground input framing camera =
    let
        dollyCamera =
            dollyShoot minDistance ground input framing camera

        finalDistance =
            V3.length (V3.sub dollyCamera.position framing.target.position)

        fovy =
            clamp 1 89 (10 * (10 - sqrt (finalDistance - minDistance)))
    in
        { dollyCamera | fovy = fovy }


zoomShoot : Ground -> Input -> Framing -> Camera -> Camera
zoomShoot ground input framing camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        -- input
        inputZoom =
            -input.y * 30 * input.dt

        inputYaw =
            -input.x * 1 * input.dt

        target =
            framing.target

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        -- New position of the camera, just tracking the target
        trackingPosition =
            V3.add target.position originalDisplacement

        newDisplacement =
            moveAroundSphere 0 inputYaw originalDisplacement

        -- The new position, relative to where the target is now
        position =
            V3.add target.position newDisplacement

        fovy =
            clamp 1 89 (camera.fovy + inputZoom)
    in
        { camera
            | position = ground.bounds 0.1 position
            , fovy = fovy
        }
            |> Camera.retarget target
            |> Camera.rollUpright


arcShoot : Ground -> Input -> Framing -> Camera -> Camera
arcShoot ground input framing camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        inputYaw =
            -input.x * 1 * input.dt

        inputPitch =
            -input.y * 1 * input.dt

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        newDisplacement =
            moveAroundSphere inputPitch inputYaw originalDisplacement

        position =
            V3.add framing.target.position newDisplacement
    in
        { camera | position = ground.bounds 0.1 position }
            |> Camera.retarget framing.target
            |> Camera.rollUpright



-- Move around a sphere centered at the origin


moveAroundSphere : Float -> Float -> Vec3 -> Vec3
moveAroundSphere dTheta dPhi pos =
    let
        fromSpherical ( r, theta, phi ) =
            V3.vec3 (r * sin theta * cos phi)
                (r * cos theta)
                (r * sin theta * sin phi)

        toSpherical v =
            let
                ( x, y, z ) =
                    V3.toTuple v

                r =
                    V3.length v

                -- sqrt(x*x + y*y + z*z)
            in
                ( r, acos (y / r), atan2 z x )

        ( r, theta, phi ) =
            toSpherical pos

        thetaNew =
            clamp 0 pi <| theta + dTheta

        phiNew =
            phi + dPhi
    in
        if r == 0 || (thetaNew == 0 || thetaNew == pi) then
            pos
        else
            fromSpherical ( r, thetaNew, phiNew )
