module Orientation exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Quaternion as Qn

type alias Orientation = Qn.Quaternion

initial : Orientation
initial = Qn.unit

fromRollPitchYaw : (Float, Float, Float) -> Orientation
fromRollPitchYaw (roll, pitch, yaw) = Qn.fromYawPitchRoll (yaw, pitch, roll)

toRollPitchYaw : Orientation -> (Float, Float, Float)
toRollPitchYaw o = let (yaw, pitch, roll) = Qn.toYawPitchRoll o in (roll, pitch, yaw)

followedBy : Orientation -> Orientation -> Orientation
followedBy = Qn.multiply

rotateBodyV : Orientation -> Vec3 -> Vec3
rotateBodyV = Qn.rotate

rotateLabV : Orientation -> Vec3 -> Vec3
rotateLabV o = Qn.rotate (Qn.negate o)

