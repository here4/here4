module Orientation exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Quaternion as Qn

type alias Orientation = Qn.Quaternion

initial : Orientation
initial = Qn.unit

-- quaternion (flight dynamics) to world
qToW : V3.Vec3 -> V3.Vec3
qToW v =
     let (x, y, z) = V3.toTuple v
     in V3.vec3 y z (-x)

-- world to quaternion (flight dynamics)
wToQ : V3.Vec3 -> V3.Vec3
wToQ v =
     let (x, y, z) = V3.toTuple v
     in V3.vec3 (-z) x y

fromVec3 : V3.Vec3 -> Orientation
fromVec3 = wToQ >> Qn.fromVec3

fromRollPitchYaw : (Float, Float, Float) -> Orientation
fromRollPitchYaw (roll, pitch, yaw) = Qn.fromYawPitchRoll (yaw, pitch, roll)

toRollPitchYaw : Orientation -> (Float, Float, Float)
toRollPitchYaw o = let (yaw, pitch, roll) = Qn.toYawPitchRoll o in (roll, pitch, yaw)

followedBy : Orientation -> Orientation -> Orientation
followedBy = Qn.multiply

rotateBodyV : Orientation -> Vec3 -> Vec3
rotateBodyV o = wToQ >> Qn.rotate o >> qToW

rotateLabV : Orientation -> Vec3 -> Vec3
rotateLabV o = wToQ >> Qn.rotate (Qn.conjugate o) >> qToW


