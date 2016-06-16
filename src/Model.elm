module Model where

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (makeRotate, transform)

import Orientation

type alias EyeLevel = Vec3 -> Float

type alias Inputs =
    { reset : Bool
    , changeVR : Bool
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
noInput = { reset = False, changeVR = False, changeCamera = False, isJumping = False, button_X = False, x=0, y=0, dt=0, mx=0, my=0, mt=0 }

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

type alias WhichVehicle = Int
vehicleBuggy = 0
vehicleBird = 1
vehicleDebug = 2

nextVehicle : WhichVehicle -> WhichVehicle
nextVehicle v = (v+1) % 3

type alias Person =
    { pos : Vec3
    , velocity : Vec3
    , orientation : Orientation.Orientation
    , vehicle : WhichVehicle
    , cameraVR : Bool
    , cameraInside : Bool
    , cameraPos : Vec3
    , cameraUp : Vec3
    }

eyeLevel : Float
eyeLevel = 1.8 -- Make this a function of Vehicle

defaultPerson : Person
defaultPerson =
    { pos = vec3 0 30 0 
    , velocity = vec3 0 0 0
    , orientation = Orientation.initial
    , vehicle = vehicleBuggy
    , cameraVR = False
    , cameraInside = True
    , cameraPos = vec3 0 eyeLevel 0
    , cameraUp = V3.j
    }

orient : Person -> Vec3 -> Vec3
orient person = Orientation.rotateBodyV person.orientation

direction : Person -> Vec3
direction person = orient person V3.k

cameraUp : Person -> Vec3
-- cameraUp person = orient person V3.j
-- cameraUp person = Qn.vrotate (Qn.negate person.orientQn) V3.j
cameraUp person = Orientation.rotateLabV person.orientation V3.j
