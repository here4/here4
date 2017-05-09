module Vehicles.LookAt exposing (lookAt)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3

import Math.Quaternion as Qn

import Model

----------------------------------------------------------------------
-- LookAt

lookAt : Model.Vehicle
lookAt =
    { init = welcome
    , move = move
    }

welcome : Model.Motion -> Model.Motion
welcome motion = motion

move : Maybe Vec3 -> Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
move focPos eyeLevel inputs motion = case focPos of
  Nothing -> motion
  Just fpos ->
    let
        -- Some point directly above a given point
        upwardsFrom p = V3.setY (V3.getY p + 1) p

        p = V3.sub fpos motion.position

        -- inputs
        upDown = V3.scale (3.0 * inputs.y) (V3.normalize p)
        inputYaw = inputs.mx * 30 * inputs.dt
        inputPitch = inputs.my * 30 * inputs.dt

        cPos = V3.add motion.position upDown

        -- Limit how close you can get. This should be a function of the size of the thing.
        closePos = if V3.length (V3.sub fpos cPos) < 3.0 then motion.position else cPos

{-
        fromSpherical (r, theta, phi) = (r * sin theta * cos phi, r * sin theta * sin phi, r * cos theta)
        toSpherical (x,y,z) = let r = sqrt(x*x+y*y+z*z) in (r, acos (z/r), atan2 y x)

        -- Move around a sphere centered at the origin
        moveAroundSphere dTheta dPhi (x,y,z) =
            let (r, theta, phi) = toSpherical (x,y,z)
            in fromSpherical (r, theta+dTheta, phi+dPhi)

        cv = closePos `V3.sub` fpos
        (cx, cy, cz) = V3.toTuple cv

        (nx, ny, nz) = moveAroundSphere inputYaw inputPitch (cx, cy, cz)

        wantPos = V3.add fpos <| V3.fromTuple (nx, ny, nz)
-}

        yaw = Qn.normalize <| Qn.fromAngleAxis inputYaw V3.j
        xzPos = Qn.vrotate yaw <| V3.sub closePos fpos
        wantPos = V3.add fpos xzPos

{-
        pitchAxis = V3.cross xzPos V3.j
        pitchQ = Qn.fromAngleAxis inputPitch pitchAxis
        wantPos = V3.add fpos <| Qn.vrotate pitchQ xzPos
-}

        unboundPos = V3.add (V3.scale 0.3 wantPos) (V3.scale 0.7 motion.position)

        u = toRecord unboundPos
        e = eyeLevel unboundPos

        newPos = if u.y < e then
                   vec3 u.x e u.z
               else
                   unboundPos

        -- Find the orientation looking at fpos from newPos
        f = V3.normalize (V3.sub fpos newPos)
        orPos = Qn.fromTo2 V3.k f

        -- Ensure the camera is in an upright plane
        -- camAxis = V3.cross f (V3.setY (V3.getY f + 1) f)
        camAxis = V3.cross f (upwardsFrom f)
        camQ = Qn.fromAngleAxis (pi/2) camAxis
        cam = V3.normalize <| Qn.vrotate camQ f

        orCam = Qn.fromTo (Qn.vrotate orPos V3.j) cam
        orientation = Qn.hamilton orCam orPos
        
    in
        { motion | position = newPos, orientation = orientation }
