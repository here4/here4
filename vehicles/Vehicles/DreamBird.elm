module Vehicles.DreamBird exposing (drive)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Orientation exposing (..)
import Body exposing (..)
import Model
import Ground exposing (Ground)


----------------------------------------------------------------------
-- DreamBird
-- | Welcome a new driver to the DreamBird


welcome : Model.Motion -> Model.Motion
welcome motion =
    motion


drive : Ground -> Model.Inputs -> Moving (HasBody a) -> Moving (HasBody a)
drive ground inputs body =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        motion0 =
            { position = body.position, orientation = body.orientation, velocity = body.velocity }

        motion =
            move ground eyeLevel inputs motion0
    in
        { body
            | position = motion.position
            , orientation = motion.orientation
            , velocity = motion.velocity
        }


move : Ground -> Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
move ground eyeLevel inputs motion =
    motion
        |> fly eyeLevel inputs
        |> flyPhysics eyeLevel inputs.dt
        |> keepWithinbounds ground



-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf


fly : Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
fly eyeLevel inputs motion =
    let
        thrust =
            2 * inputs.y

        yaw =
            0.5 * inputs.x * inputs.dt

        pitch =
            -0.8 * inputs.my * inputs.dt

        roll =
            1.2 * inputs.mx * inputs.dt

        orpy =
            fromRollPitchYaw ( roll, pitch, yaw )

        orientation =
            followedBy motion.orientation orpy

        orient =
            rotateBodyV orientation

        dv =
            V3.scale (5 * thrust * inputs.dt) <| orient V3.k

        du =
            V3.scale (2 * thrust * inputs.dt) <| orient V3.j

        dv_ =
            add dv du

        vel =
            add (V3.scale 0.8 dv_) (V3.scale 0.95 motion.velocity)
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


keepWithinbounds ground motion =
    { motion | position = ground.bounds motion.position }
