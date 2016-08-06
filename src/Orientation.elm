module Orientation exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as M4
import Math.Quaternion as Qn

type alias Orientation = Qn.Quaternion

initial : Orientation
initial = Qn.unit

fromRollPitchYaw : (Float, Float, Float) -> Orientation
fromRollPitchYaw = Qn.fromEuler

toRollPitchYaw : Orientation -> (Float, Float, Float)
toRollPitchYaw = Qn.toEuler

fromMatrix : Mat4 -> Orientation
-- fromMatrix mat = Qn.fromVec3 <| M4.transform mat V3.i
fromMatrix mat = Qn.fromWorldVec3 <| M4.transform mat V3.k

fromVec3 : Vec3 -> Orientation
fromVec3 = Qn.fromWorldVec3

followedBy : Orientation -> Orientation -> Orientation
followedBy = Qn.hamilton

-- TODO: translate to/from flight dynamics / our world coordinates
-- ie. our quaternions work on X=ahead, Y=right, Z=down (RHS)
-- but our world is X=right, Y=up, Z=ahead (LHS)

-- TODO: PLAYTEST THIS!

rotateBodyV : Orientation -> Vec3 -> Vec3
rotateBodyV = Qn.vrotate
-- rotateBodyV = Qn.worldVRotate

rotateLabV : Orientation -> Vec3 -> Vec3
rotateLabV o = Qn.vrotate (Qn.negate o) 
-- rotateLabV o = Qn.worldVRotate (Qn.negate o) 

