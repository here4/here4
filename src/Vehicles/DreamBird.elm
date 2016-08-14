module Vehicles.DreamBird exposing (move, welcome)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Orientation exposing (..)

import Model

----------------------------------------------------------------------
-- DreamBird

-- | Welcome a new driver to the DreamBird
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

        -- orientQn = Qn.hamilton player.orientQn (Qn.fromEuler (roll, pitch, yaw))
        -- orient = Qn.vrotate orientQn
        orpy = fromRollPitchYaw (roll, pitch, yaw)
        orientation = player.orientation `followedBy` orpy
        orient = rotateBodyV orientation

        dv = V3.scale (50 * thrust * inputs.dt) <| orient V3.k
        du = V3.scale (20 * thrust * inputs.dt) <| orient V3.j
        dv' = dv `add` du

        vel = (V3.scale 0.8 dv') `add` (V3.scale 0.95 player.velocity)
        
    in
        { player | orientation = orientation
                 , velocity = vel
        }

flyPhysics : Model.EyeLevel -> Float -> Model.Player -> Model.Player
flyPhysics eyeLevel dt player =
    let pos = player.pos `add` V3.scale dt player.velocity
        p = toRecord pos
        e = eyeLevel pos

        (pos', dv) = if p.y < e then
                         (vec3 p.x e p.z, vec3 0 0 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { player | pos = pos', velocity = player.velocity `add` dv }

