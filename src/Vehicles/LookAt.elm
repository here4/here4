module Vehicles.LookAt exposing (move, welcome)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import Orientation exposing (..)
import Util exposing (v3_clamp)

import Math.Quaternion as Qn

import Array2D exposing (Array2D)
import Model

import Debug

----------------------------------------------------------------------
-- LookAt

welcome : Model.Person -> Model.Person
welcome person = person

move : Model.EyeLevel -> Model.Inputs -> Maybe Vec3 -> Model.Person -> Model.Person
move eyeLevel inputs focPos person = case focPos of
  Nothing -> person
  Just fpos ->
    let
        -- Some point directly above a given point
        upwardsFrom p = V3.setY (V3.getY p + 1) p

        p = fpos `V3.sub` person.pos

        -- inputs
        upDown = V3.scale (3.0 * inputs.y) (V3.normalize p)
        inputYaw = inputs.mx * 30 * inputs.dt
        inputPitch = inputs.my * 30 * inputs.dt

        cPos = person.pos `V3.add` upDown

        -- Limit how close you can get. This should be a function of the size of the thing.
        closePos = if V3.length (fpos `V3.sub` cPos) < 3.0 then person.pos else cPos

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

        unboundPos = V3.scale 0.3 wantPos `V3.add` V3.scale 0.7 person.pos

        u = toRecord unboundPos
        e = eyeLevel unboundPos

        newPos = if u.y < e then
                   vec3 u.x e u.z
               else
                   unboundPos

        -- Find the orientation looking at fpos from newPos
        f = V3.normalize (fpos `V3.sub` newPos)
        orPos = Qn.fromTo2 V3.k f

        -- Ensure the camera is in an upright plane
        -- camAxis = V3.cross f (V3.setY (V3.getY f + 1) f)
        camAxis = V3.cross f (upwardsFrom f)
        camQ = Qn.fromAngleAxis (pi/2) camAxis
        cam = V3.normalize <| Qn.vrotate camQ f

        orCam = Qn.fromTo (Qn.vrotate orPos V3.j) cam
        orientation = Qn.hamilton orCam orPos
        
    in
        { person | pos = newPos, orientation = orientation }
