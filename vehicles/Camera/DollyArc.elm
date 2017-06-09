module Camera.DollyArc exposing (dolly, dollyArc)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Camera exposing (Camera, Target)
import Ground exposing (Ground)
import Model


dolly : Ground -> Model.Inputs -> Target -> Camera -> Camera
dolly ground inputs target camera =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        -- inputs
        inputNearFar =
            -inputs.y * 30 * inputs.dt

        -- The original displacement of the camera, relative to where the target was
        originalDisplacement =
            V3.sub camera.position camera.target.position

        -- New position of the camera, just tracking the target
        trackingPosition =
            V3.add originalDisplacement target.position

        -- Vector to move closer to target
        moveCloser =
            V3.scale inputNearFar (V3.normalize originalDisplacement)

        -- The new position, relative to where the target is now
        rawClosePos =
            V3.add trackingPosition moveCloser

        -- Limit how close you can get. This should be a function of the size of the thing.
        closePos =
            if V3.length (V3.sub target.position rawClosePos) < 3.0 then
                trackingPosition
            else
                rawClosePos

    in
        { camera | position = closePos, target = target }


dollyArc : Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
dollyArc target ground inputs motion =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        -- inputs
        inputNearFar =
            inputs.y * 30 * inputs.dt

        inputYaw =
            inputs.mx * 30 * inputs.dt

        inputPitch =
            inputs.my * 30 * inputs.dt

        -- The original position, relative from target
        p =
            V3.sub target motion.position

        -- Some point directly above a given point
        upwardsFrom p =
            V3.setY (V3.getY p + 1) p

        -- Vector to move closer to target
        moveCloser =
            V3.scale inputNearFar (V3.normalize p)

        cPos =
            V3.add motion.position moveCloser

        -- Limit how close you can get. This should be a function of the size of the thing.
        closePos =
            if V3.length (V3.sub target cPos) < 3.0 then
                motion.position
            else
                cPos

        fromSpherical (r, theta, phi) = (r * sin theta * cos phi, r * sin theta * sin phi, r * cos theta)
        toSpherical (x,y,z) = let r = sqrt(x*x+y*y+z*z) in (r, acos (z/r), atan2 y x)

        -- Move around a sphere centered at the origin
        moveAroundSphere dTheta dPhi (x,y,z) =
            let (r, theta, phi) = toSpherical (x,y,z)
            in fromSpherical (r, theta+dTheta, phi+dPhi)

        cv = V3.sub closePos target
        (cx, cy, cz) = V3.toTuple cv

        (nx, ny, nz) = moveAroundSphere inputYaw inputPitch (cx, cy, cz)

        wantPos = V3.add target <| V3.fromTuple (nx, ny, nz)

{-
        yaw =
            Orientation.fromAngleAxis inputYaw V3.j

        xzPos =
            Orientation.rotateBodyV yaw <| V3.sub closePos target

        wantPos =
            V3.add target xzPos
-}

        {-
           pitchAxis = V3.cross xzPos V3.j
           pitchQ = Qn.fromAngleAxis inputPitch pitchAxis
           wantPos = V3.add target <| Qn.vrotate pitchQ xzPos
        -}
        unboundPos =
            V3.add (V3.scale 0.3 wantPos) (V3.scale 0.7 motion.position)

        u =
            toRecord unboundPos

        e =
            eyeLevel unboundPos

        newPos =
            if u.y < e then
                vec3 u.x e u.z
            else
                unboundPos

        -- Find the orientation looking at target from newPos
        f =
            V3.normalize (V3.sub target newPos)

        orPos =
            Orientation.fromTo V3.k f
            -- Orientation.fromTo newPos target

        -- Ensure the camera is in an upright plane
        -- camAxis = V3.cross f (V3.setY (V3.getY f + 1) f)
        camAxis =
            V3.cross f (upwardsFrom f)

        camQ =
            Orientation.fromAngleAxis (pi / 2) camAxis

        cam =
            Orientation.rotateBodyV camQ f

        orCam =
            Orientation.fromTo (Orientation.rotateBodyV orPos V3.j) cam

        orientation =
            Orientation.followedBy orCam orPos
    in
        { motion | position = newPos, orientation = orientation }
