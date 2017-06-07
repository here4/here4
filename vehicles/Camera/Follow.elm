module Camera.Follow exposing (follow)

import Math.Vector3 as V3 exposing (..)
import Orientation as Orientation

import Body exposing (Moving)
import Ground exposing (Ground)
import Model

aboveGround : Model.EyeLevel -> Vec3 -> Vec3
aboveGround eyeLevel pos =
    let
        p =
            toRecord pos

        e =
            eyeLevel pos
    in
        if p.y < e then
            vec3 p.x e p.z
        else
            pos

follow : Ground -> Moving a -> Vec3
follow ground motion =
    let
        eyeLevel pos =
            Model.eyeLevel + ground.elevation pos

        behind =
            sub motion.position (V3.scale 17 (Model.direction motion))

        p =
            toRecord motion.position

        newCameraPos =
            if p.y < Model.eyeLevel then
                behind
            else
                add (vec3 0 6 0) behind

        cameraPos =
            aboveGround eyeLevel newCameraPos
    in
        ground.bounds cameraPos
