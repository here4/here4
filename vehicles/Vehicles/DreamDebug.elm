module Vehicles.DreamDebug exposing (move, welcome)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Orientation exposing (..)

import Model

----------------------------------------------------------------------
-- DreamDebug

-- | Welcome a new driver to debug
welcome : Model.Player -> Model.Player
welcome player = player

move : Model.EyeLevel -> Model.Inputs -> Model.Player -> Model.Player
move eyeLevel inputs player = 
    player |> fly eyeLevel inputs
           |> flyPhysics eyeLevel inputs.dt

-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf
fly : Model.EyeLevel -> Model.Inputs -> Model.Player -> Model.Player
fly eyeLevel inputs player =
    let
        thrust = inputs.y

        yaw   = -5 * inputs.x  * inputs.dt
        pitch =  4 * inputs.my * inputs.dt
        roll  =  6 * inputs.mx * inputs.dt

        orpy = fromRollPitchYaw (roll, pitch, yaw)
        orientation = followedBy player.orientation orpy
        orient = rotateBodyV orientation

        dv = V3.scale (50 * thrust * inputs.dt) <| orient V3.k

        vel = add (V3.scale 0.8 dv) (V3.scale 0.95 player.velocity)
        
    in
        { player | orientation = orientation
                 , velocity = vel
        }

flyPhysics : Model.EyeLevel -> Float -> Model.Player -> Model.Player
flyPhysics eyeLevel dt player =
    let pos = add player.pos (V3.scale dt player.velocity)
        p = toRecord pos
        e = eyeLevel pos

        (pos_, dv) = if p.y < e then
                         (vec3 p.x e p.z, vec3 0 0 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { player | pos = pos_, velocity = add player.velocity dv }

