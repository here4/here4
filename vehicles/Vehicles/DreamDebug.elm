module Vehicles.DreamDebug exposing (dreamDebug)

import Here4.Orientation as Orientation exposing (..)
import Here4.Model as Model
import Math.Vector3 exposing (..)
import Math.Vector3 as V3


----------------------------------------------------------------------
-- DreamDebug


move : Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
move eyeLevel inputs motion =
    motion
        |> fly eyeLevel inputs
        |> flyPhysics eyeLevel inputs.dt



-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf


fly : Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
fly eyeLevel inputs motion =
    let
        thrust =
            inputs.y

        yaw =
            -5 * inputs.x * inputs.dt

        pitch =
            4 * inputs.my * inputs.dt

        roll =
            6 * inputs.mx * inputs.dt

        orpy =
            fromRollPitchYaw ( roll, pitch, yaw )

        orientation =
            followedBy motion.orientation orpy

        orient =
            rotateBodyV orientation

        dv =
            V3.scale (50 * thrust * inputs.dt) <| orient V3.k

        vel =
            add (V3.scale 0.8 dv) (V3.scale 0.95 motion.velocity)
    in
        { motion
            | orientation = orientation
            , velocity = vel
        }


flyPhysics : Model.EyeLevel -> Float -> Model.Motion -> Model.Motion
flyPhysics eyeLevel dt motion =
    let
        pos =
            add motion.position (V3.scale dt motion.velocity)

        p =
            toRecord pos

        e =
            eyeLevel pos

        ( pos_, dv ) =
            if p.y < e then
                ( vec3 p.x e p.z, vec3 0 0 0 )
            else
                ( pos, vec3 0 0 0 )
    in
        { motion | position = pos_, velocity = add motion.velocity dv }
