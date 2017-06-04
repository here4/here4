module Vehicles.DreamBuggy exposing (drive)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Orientation exposing (..)

import Body exposing (..)
import Model
import Ground exposing (Ground)

----------------------------------------------------------------------
-- DreamBuggy

-- | Welcome a new driver to the DreamBuggy
welcome : Model.Motion -> Model.Motion
welcome motion = { motion | orientation = clampBuggy motion.orientation }

drive : Ground -> Float -> Model.Inputs -> Moving (HasBody a) -> Moving (HasBody a)
drive ground speed inputs body =
    let eyeLevel pos = 1.8 + ground.elevation pos
        motion0 = { position = body.position, orientation = body.orientation, velocity = body.velocity }
        motion = move ground speed eyeLevel inputs motion0
    in
        { body | position = motion.position
               , orientation = motion.orientation
               , velocity = motion.velocity
        }

move : Ground -> Float -> Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
move terrain speed eyeLevel inputs motion =
    motion |> turn eyeLevel inputs.mx inputs.my
           |> goForward eyeLevel speed inputs
           |> gravity eyeLevel inputs.dt
           |> physics eyeLevel inputs.dt
           |> keepWithinbounds terrain

clampBuggy : Orientation -> Orientation
clampBuggy o = o
{-
    let (roll, pitch, yaw) = Orientation.toRollPitchYaw o
        roll_ = clamp (degrees -10) (degrees 10) (roll/2)
        pitch_ = clamp (degrees -15) (degrees 15) (pitch/2)
    in Orientation.fromRollPitchYaw (roll_, pitch_, yaw)
-}

flatten : Vec3 -> Vec3
flatten v =
    let r = V3.toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : Model.EyeLevel -> Float -> Float -> Model.Motion -> Model.Motion
turn eyeLevel dx dy motion =
    let
        motionY = eyeLevel motion.position
        frontTireY = eyeLevel (add motion.position (rotateBodyV motion.orientation (vec3 0 0 1)))
        rightTireY = eyeLevel (add motion.position (rotateBodyV motion.orientation (vec3 1 0 0)))
        leftTireY = eyeLevel (add motion.position (rotateBodyV motion.orientation (vec3 -1 0 0)))
        tirePitch = 0 -- atan (-(frontTireY - motionY)/0.01)
        tireRoll  = atan ((rightTireY - leftTireY)/0.1)
        (yaw, pitch, roll) =
            -- if getY motion.position > (eyeLevel motion.position) + 5 then -- spin if in the air
            --     (dx * 5, dy*0.1, 0)
            -- else
                -- (dx*0.5, (tirePitch+dy)*0.05, tireRoll*0.05)
                (dx*0.5, 0, 0)

        orpy = fromRollPitchYaw (roll, pitch, yaw)
        orientation = clampBuggy (followedBy motion.orientation orpy)
    in
        { motion | orientation = orientation }

goForward : Model.EyeLevel -> Float -> { a | x:Float, y:Float, dt:Float } -> Model.Motion -> Model.Motion
goForward eyeLevel speed inputs motion =
  -- if getY motion.position > eyeLevel motion.position then motion else
    let moveDir = normalize (flatten (Model.direction motion))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * inputs.y) moveDir
        strafe = V3.scale (8.0 * inputs.x) strafeDir

        -- e = (eyeLevel motion.position) / 80.0 -- placement.yMult
        e = (eyeLevel motion.position) / 20.0

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
        { motion | velocity = adjustVelocity maxSpeed friction (add move strafe) inputs.dt motion.velocity }

adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0-(friction*dt)) v)

physics : Model.EyeLevel -> Float -> Model.Motion -> Model.Motion
physics eyeLevel dt motion =
    let pos = add motion.position (Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity))
        p = V3.toRecord pos
        e = eyeLevel pos
        vy0 = getY motion.velocity

        (pos_, dv) = if p.y < e then
                         let vy = if ((e < (0.8*80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10*dt) then
                             clamp 0 10 (V3.length motion.velocity * (e-p.y)*dt*5) else 0
                         in (vec3 p.x e p.z, vec3 0 vy 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { motion | position = pos_, velocity = add motion.velocity dv }

-- | Clamp a vector to be no longer than len
v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v = if V3.length v <= len then v else V3.scale len (V3.normalize v)

keepWithinbounds terrain motion = { motion | position = terrain.bounds motion.position }

gravity : Model.EyeLevel -> Float -> Model.Motion -> Model.Motion
gravity eyeLevel dt motion =
  if getY motion.position <= eyeLevel motion.position then motion else
    let v = V3.toRecord motion.velocity
    in
        { motion | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
