module Here4.Orientation exposing (..)

import Geometry.Projection exposing (..)
import Math.Matrix4 as M4
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Quaternion as Qn
import Quaternion.Internal as QnI


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


fromAngleAxis : Float -> V3.Vec3 -> Orientation
fromAngleAxis angle axis =
    Qn.fromAngleAxis angle (wToQ axis)


getAxis : Orientation -> V3.Vec3
getAxis =
    Qn.getAxis >> qToW


getAngle : Orientation -> Float
getAngle =
    Qn.getAngle


fromTo : V3.Vec3 -> V3.Vec3 -> Orientation
fromTo u v =
    Qn.fromTo (wToQ u) (wToQ v)


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



-- wind?


followedBy : Orientation -> Orientation -> Orientation
followedBy =
    Qn.multiply


unwind : Orientation -> Orientation -> Orientation
unwind q1 q2 =
    Qn.multiply q1 (Qn.conjugate q2)


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
            M4.rotate angle (Qn.getAxis o |> qToW)


rotateLabM4 : Orientation -> M4.Mat4 -> M4.Mat4
rotateLabM4 o =
    let
        angle =
            Qn.getAngle o
    in
        if angle == 0 then
            identity
        else
            M4.rotate angle (Qn.getAxis o |> qToW |> V3.negate)


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


slerp : Float -> Orientation -> Orientation -> Orientation
slerp t o1 o2 =
    Qn.slerp t o1 o2


-- | Roll to upright


rollUpright : Orientation -> Orientation
rollUpright =
    rollTo V3.j



-- | Project rotated V3.j onto plane formed by V3.j and rotated V3.k


rollTo : V3.Vec3 -> Orientation -> Orientation
rollTo targetUp o =
    let
        -- Camera forward vector of the original orientation
        fwd =
            rotateBodyV o V3.k

        -- Camera up vector of the original orientation
        up =
            rotateBodyV o V3.j

        -- Projection of the up vector onto the plane formed by
        -- the forward vector and the Y axis
        upProj =
            projectPlane targetUp fwd up

        -- Helper function to flip a vector if it is pointing downwards
        -- We use this to ensure we don't end up with an upside-down camera
        positiveY v =
            if V3.getY v >= 0 then
                v
            else
                V3.negate v

        -- Orientation that rolls the camera back to an upright position
        roll =
            fromTo up (positiveY upProj)
    in
        -- Apply the uprighting roll to the original orientation
        followedBy roll o



-- | Pitch to upright


pitchUpright : Orientation -> Orientation
pitchUpright =
    pitchTo V3.j



-- | Project rotated V3.j onto plane formed by V3.j and rotated V3.i


pitchTo : V3.Vec3 -> Orientation -> Orientation
pitchTo targetUp o =
    let
        -- Right vector of the original orientation
        right =
            rotateBodyV o V3.i

        -- Up vector of the original orientation
        up =
            rotateBodyV o V3.j

        -- Projection of the up vector onto the plane formed by
        -- the forward vector and the Y axis
        upProj =
            projectPlane targetUp right up

        -- Helper function to flip a vector if it is pointing downwards
        -- We use this to ensure we don't end up with an upside-down camera
        positiveY v =
            if V3.getY v >= 0 then
                v
            else
                V3.negate v

        -- Orientation that rolls the camera back to an upright position
        pitch =
            fromTo up (positiveY upProj)
    in
        -- Apply the uprighting roll to the original orientation
        followedBy pitch o
