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
        closePos = if V3.length (fpos `V3.sub` cPos) < 3.0 then person.pos else cPos

        yaw = Qn.normalize <| Qn.fromAngleAxis (inputs.mx * 30 * inputs.dt) V3.j
        pitch = Qn.normalize <| Qn.fromAngleAxis (inputs.my * 30 * inputs.dt) V3.i
        wantPos = -- V3.setY (V3.getY fpos)
                  V3.add fpos
                  <| Qn.vrotate pitch
                  <| Qn.vrotate yaw
                  <| V3.sub closePos fpos

        unboundPos = V3.scale 0.3 wantPos `V3.add` V3.scale 0.7 person.pos

        u = toRecord unboundPos
        e = eyeLevel unboundPos

        newPos = if u.y < e then
                   vec3 u.x e u.z
               else
                   unboundPos

        f = fpos `V3.sub` newPos

        orientation = Qn.fromTo2 V3.k f
        
    in
        { person | pos = newPos, orientation = orientation }
