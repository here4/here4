module Camera.DollyArc exposing (dolly, arc)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Camera exposing (..)
import Camera.Util as Camera
import Ground exposing (Ground)
import Model

dolly : Shot
dolly =
    { label = "Dolly"
    , init = dollyInit
    , shoot = dollyShoot
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
        target = camera.target
        position =
            sub target.position (V3.scale 17 (Model.direction target))
    in
        { camera | position = position }
        |> Camera.retarget camera.target
        |> Camera.rollUpright

dollyShoot : Ground -> Input -> Target -> Camera -> Camera
dollyShoot ground input target camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        -- input
        inputNearFar =
            -input.y * 30 * input.dt

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        -- New position of the camera, just tracking the target
        trackingPosition =
            V3.add target.position originalDisplacement

        -- Vector to move closer to target
        moveCloser =
            V3.scale inputNearFar (V3.normalize originalDisplacement)

        -- The new position, relative to where the target is now
        newPosition =
            V3.add trackingPosition moveCloser

        -- Displacement looking at target from newPosition
        newDisplacement =
            V3.sub target.position newPosition

        -- Limit how close you can get. This should be a function of the size of the thing.
        position =
            if V3.length newDisplacement < 3.0 then
                trackingPosition
            else
                newPosition

    in
        { camera | position = position }
        |> Camera.retarget target
        |> Camera.rollUpright



arcShoot : Ground -> Input -> Target -> Camera -> Camera
arcShoot ground input target camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        fromSpherical (r, theta, phi) =
            V3.vec3 (r * sin theta * cos phi)
                    (r * cos theta)
                    (r * sin theta * sin phi)

        toSpherical v =
            let
                (x,y,z) = V3.toTuple v
                r = V3.length v -- sqrt(x*x + y*y + z*z)
            in
                (r, acos (y/r), atan2 z x)

        -- Move around a sphere centered at the origin
        moveAroundSphere dTheta dPhi pos =
            let (r, theta, phi) = toSpherical pos
                thetaNew = clamp 0 pi <| theta + dTheta
                phiNew = phi + dPhi
            in
                if r == 0 || (thetaNew == 0 || thetaNew == pi) then
                    pos
                else
                    fromSpherical (r, thetaNew, phiNew)

        inputYaw =
            -input.x * 1 * input.dt

        inputPitch =
            -input.y * 1 * input.dt

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        newDisplacement =
            moveAroundSphere inputPitch inputYaw originalDisplacement

        position = V3.add target.position newDisplacement
    in
        { camera | position = position }
        |> Camera.retarget target
        |> Camera.rollUpright
