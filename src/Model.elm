module Model where

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (makeRotate, transform)
import Math.Quaternion as Qn

type alias EyeLevel = Vec3 -> Float

type alias Inputs =
    { reset : Bool
    , changeCamera : Bool
    , isJumping: Bool
    , button_X: Bool
    , x: Float
    , y: Float
    , dt: Float
    , mx: Float
    , my: Float
    , mt: Float
    }

noInput : Inputs
noInput = { reset = False, changeCamera = False, isJumping = False, button_X = False, x=0, y=0, dt=0, mx=0, my=0, mt=0 }

-- TODO: Make a new model type with both Person and Things
-- but the Things are passed in and can be added/subtracted
-- ... and maybe set portals separately?
-- ... how to handle collisions between person and things?

{-
type World =
    { person : Person
    , things : [Thing]
    }
-}

type alias Person =
    { pos : Vec3
    , velocity : Vec3
    , orientQn: Qn.Quaternion
    , flying : Bool
    , cameraInside : Bool
    , cameraPos : Vec3
    , cameraUp : Vec3
    }

eyeLevel : Float
eyeLevel = 1.8

defaultPerson : Person
defaultPerson =
    { pos = vec3 0 30 0 
    , velocity = vec3 0 0 0
    , orientQn = Qn.unit
    , flying = False
    , cameraInside = True
    , cameraPos = vec3 0 eyeLevel 0
    , cameraUp = V3.j
    }

orient : Person -> Vec3 -> Vec3
orient person = Qn.vrotate person.orientQn

direction : Person -> Vec3
direction person = orient person V3.k

cameraUp : Person -> Vec3
cameraUp person = orient person V3.j
