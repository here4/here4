module Vehicles.DreamBuggy exposing (move, welcome)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Orientation exposing (..)
import Util exposing (v3_clamp)

import Model

----------------------------------------------------------------------
-- DreamBuggy

-- | Welcome a new driver to the DreamBuggy
welcome : Model.Player -> Model.Player
welcome player = { player | orientation = clampBuggy player.orientation }

move : Model.EyeLevel -> Model.Inputs -> Model.Player -> Model.Player
move eyeLevel inputs player =
    player |> turn eyeLevel inputs.mx inputs.my
           |> drive eyeLevel inputs
           |> physics eyeLevel inputs.dt

clampBuggy : Orientation -> Orientation
clampBuggy o =
    let (roll, pitch, yaw) = Orientation.toRollPitchYaw o
        roll' = clamp (degrees -10) (degrees 10) (roll/2)
        pitch' = clamp (degrees -15) (degrees 15) (pitch/2)
    in Orientation.fromRollPitchYaw (roll', pitch', yaw)

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : Model.EyeLevel -> Float -> Float -> Model.Player -> Model.Player
turn eyeLevel dx dy player =
    let
        (roll0, pitch0, yaw0) = toRollPitchYaw player.orientation
        playerY = eyeLevel player.pos
        frontTireY = eyeLevel (player.pos `add` (rotateBodyV player.orientation (vec3 0 0 1)))
        rightTireY = eyeLevel (player.pos `add` (rotateBodyV player.orientation (vec3 1 0 0)))
        leftTireY = eyeLevel (player.pos `add` (rotateBodyV player.orientation (vec3 -1 0 0)))
        tirePitch = 0 -- atan (-(frontTireY - playerY)/0.01)
        tireRoll  = atan ((rightTireY - leftTireY)/0.1)
        (yaw, pitch, roll) =
            if getY player.pos > (eyeLevel player.pos) + 5 then
                (yaw0-(dx * 5), pitch0*0.9 + dy*0.1, 0)
            else
                (yaw0-dx, pitch0*0.95 + (tirePitch+dy)*0.05, roll0*0.95 + (tireRoll*0.05))
                -- (yaw0-dx, pitch0*0.05 + tirePitch*0.95, tireRoll)

        orientation = clampBuggy (fromRollPitchYaw (roll, pitch, yaw))
    in
        { player | orientation = orientation }

drive : Model.EyeLevel -> { a | x:Float, y:Float, dt:Float } -> Model.Player -> Model.Player
drive eyeLevel inputs player =
  -- if getY player.pos > eyeLevel player.pos then player else
    let moveDir = normalize (flatten (Model.direction player))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * inputs.y) moveDir
        strafe = V3.scale (8.0 * inputs.x) strafeDir

        -- e = (eyeLevel player.pos) / 80.0 -- placement.yMult
        e = (eyeLevel player.pos) / 20.0

        friction = if e > 0.8 then -0.1
                   else if e < 0.0 then 0.8
                   else if e < 0.1 then 0.6
                   else if e < 0.15 then 0.5
                   else 0.2

        maxSpeed = if e > 0.8 then 30
                   else if e < 0.0 then 3
                   else if e < 0.1 then 10
                   else if e < 0.15 then 15
                   else 20
    in
        { player | velocity = adjustVelocity maxSpeed friction (move `add` strafe) inputs.dt player.velocity }

adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0-(friction*dt)) v)

physics : Model.EyeLevel -> Float -> Model.Player -> Model.Player
physics eyeLevel dt player =
    let pos = player.pos `add` V3.scale dt player.velocity
        p = toRecord pos
        e = eyeLevel pos
        vy0 = getY player.velocity

        (pos', dv) = if p.y < e then
                         let vy = if ((e < (0.8*80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10*dt) then
                             clamp 0 10 (V3.length player.velocity * (e-p.y)*dt*5) else 0
                         in (vec3 p.x e p.z, vec3 0 vy 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { player | pos = pos', velocity = player.velocity `add` dv }
