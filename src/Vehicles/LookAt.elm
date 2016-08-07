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
        p = fpos `V3.sub` person.pos

        upDown = V3.scale (3.0 * inputs.y) (V3.normalize p)

        cPos = person.pos `V3.add` upDown

        -- Limit how close you can get. This should be a function of the size of the thing.
        closePos = if V3.length (fpos `V3.sub` cPos) < 3.0 then person.pos else cPos

        yaw = Qn.normalize <| Qn.fromAngleAxis (inputs.mx * 30 * inputs.dt) V3.j
        xzPos = Qn.vrotate yaw <| V3.sub closePos fpos

        pitchDir = if (V3.getZ xzPos < 0) then 1 else -1
        pitch = Qn.normalize <| Qn.fromAngleAxis (pitchDir * inputs.my * 30 * inputs.dt) V3.i
        wantPos = -- V3.setY (V3.getY fpos)
                  V3.add fpos
                  <| Qn.vrotate pitch xzPos

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
        camAxis = V3.cross f (V3.setY (V3.getY f + 1) f)
        camQ = Qn.fromAngleAxis (pi/2) camAxis
        cam = V3.normalize <| Qn.vrotate camQ f

        orCam = Qn.fromTo (Qn.vrotate orPos V3.j) cam
        orientation = Qn.hamilton orCam orPos
        
    in
        { person | pos = newPos, orientation = orientation }
