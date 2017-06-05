module Orientation exposing (..)

import Math.Matrix4 as M4
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Quaternion as Qn


type alias Orientation =
    Qn.Quaternion


initial : Orientation
initial =
    Qn.unit



-- quaternion (flight dynamics) to world


qToW : V3.Vec3 -> V3.Vec3
qToW v =
    let
        ( x, y, z ) =
            V3.toTuple v
    in
        V3.vec3 y z (-x)



-- world to quaternion (flight dynamics)


wToQ : V3.Vec3 -> V3.Vec3
wToQ v =
    let
        ( x, y, z ) =
            V3.toTuple v
    in
        V3.vec3 (-z) x y


fromVec3 : V3.Vec3 -> Orientation
fromVec3 =
    wToQ >> Qn.fromVec3


fromRollPitchYaw : ( Float, Float, Float ) -> Orientation
fromRollPitchYaw ( roll, pitch, yaw ) =
    Qn.fromYawPitchRoll ( yaw, pitch, roll )


toRollPitchYaw : Orientation -> ( Float, Float, Float )
toRollPitchYaw o =
    let
        ( yaw, pitch, roll ) =
            Qn.toYawPitchRoll o
    in
        ( roll, pitch, yaw )


followedBy : Orientation -> Orientation -> Orientation
followedBy =
    Qn.multiply


rotateBodyV : Orientation -> Vec3 -> Vec3
rotateBodyV o =
    wToQ >> Qn.rotate o >> qToW


rotateLabV : Orientation -> Vec3 -> Vec3
rotateLabV o =
    wToQ >> Qn.rotate (Qn.conjugate o) >> qToW


rotateBodyM4 : Orientation -> M4.Mat4 -> M4.Mat4
rotateBodyM4 o =
    let
        angle =
            Qn.getAngle o
    in
        if angle == 0 then
            identity
        else
            M4.rotate (2 * angle) (Qn.getAxis o |> qToW)


rotateLabM4 : Orientation -> M4.Mat4 -> M4.Mat4
rotateLabM4 o =
    let
        angle =
            Qn.getAngle o
    in
        if angle == 0 then
            identity
        else
            M4.rotate (2 * angle) (Qn.getAxis o |> qToW |> V3.negate)


toMat4 : Orientation -> M4.Mat4
toMat4 =
    let
        -- fromFlightDynamics = M4.makeBasis (V3.negate V3.k) V3.i (V3.negate V3.j)
        -- fromFlightDynamics = M4.makeBasis V3.j (V3.negate V3.k) (V3.negate V3.i)
        flipY =
            M4.makeBasis V3.i (V3.negate V3.j) V3.k

        -- fromFlightDynamics = M4.identity
    in
        -- Qn.toMat4 >> M4.mul flipY
        Qn.toMat4
