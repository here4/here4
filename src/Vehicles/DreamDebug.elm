module Vehicles.DreamDebug exposing (move, welcome)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Orientation exposing (..)

import Model

----------------------------------------------------------------------
-- DreamDebug

move : Model.EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
move eyeLevel inputs person = 
    person |> fly eyeLevel inputs
           |> flyPhysics eyeLevel inputs.dt

-- | Welcome a new driver to debug
welcome : Model.Person -> Model.Person
welcome person = person

-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf
fly : Model.EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
fly eyeLevel inputs person =
    let
        thrust = inputs.y

        yaw   = -5 * inputs.x  * inputs.dt
        pitch =  4 * inputs.my * inputs.dt
        roll  =  6 * inputs.mx * inputs.dt

        orpy = fromRollPitchYaw (roll, pitch, yaw)
        orientation = person.orientation `followedBy` orpy
        orient = rotateBodyV orientation

        dv = V3.scale (50 * thrust * inputs.dt) <| orient V3.k

        vel = (V3.scale 0.8 dv) `add` (V3.scale 0.95 person.velocity)
        
    in
        { person | orientation = orientation
                 , velocity = vel
        }

flyPhysics : Model.EyeLevel -> Float -> Model.Person -> Model.Person
flyPhysics eyeLevel dt person =
    let pos = person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos

        (pos', dv) = if p.y < e then
                         (vec3 p.x e p.z, vec3 0 0 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { person | pos = pos', velocity = person.velocity `add` dv }

