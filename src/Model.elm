module Model exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Time exposing (..)
import Task exposing (Task)
import Window

import Orientation
import Thing exposing (..)
import Things.Terrain exposing (Terrain)

import Gamepad exposing (Gamepad, gamepads)

{-| Every half a second there's an event coming through;
these are all the valid actions we could receive.
# Move - the user is trying to jump using the space key, move using the
arrow keys, or the window is being resized.
# TextureLoaded - a texture has been loaded across the wire
-}

type Msg worldMsg
    = KeyChange (Keys -> Keys)
    | MouseMove MouseMovement
    | GamepadUpdate (List Gamepad)
    | LockRequest Bool
    | LockUpdate Bool
    | Animate Time
    | Resize Window.Size
    | WorldMessage worldMsg

type alias WhichVehicle = Int
vehicleBuggy = 0
vehicleBird = 1
vehicleDebug = 2

nextVehicle : WhichVehicle -> WhichVehicle
nextVehicle v = (v+1) % 3

type alias World =
    { things : List Thing
    , terrain : Terrain
    , skybox : Thing
    }

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

type Eye = OneEye | LeftEye | RightEye

type alias EyeLevel = Vec3 -> Float

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

type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }

type alias Inputs =
    { reset : Bool
    , changeVR : Bool
    , changeCamera : Bool
    , isJumping: Bool
    , button_X: Bool
    , x: Float
    , y: Float
    , mx: Float
    , my: Float
    , dt: Float
    }

noInput : Inputs
noInput = { reset = False
          , changeVR = False
          , changeCamera = False
          , isJumping = False
          , button_X = False
          , x = 0
          , y = 0
          , mx = 0
          , my = 0
          , dt = 0
          }

{-| This type is returned by the fullscreen JS api in PointerLock.js
for mouse movement -}
type alias MouseMovement = (Int, Int)

{-| This is the application's Model data structure -}
type alias Model worldModel =
    { numPlayers : Int
    , person : Person
    , player2 : Person
    , globalTime : Time
    , maybeWindowSize : Maybe Window.Size
    , keys : Keys
    , gamepadIds : List String
    , inputs : Inputs
    , inputs2 : Inputs
    , wantToBeLocked : Bool
    , isLocked : Bool
    , message : String
    , worldModel : worldModel
    }

type alias Args =
    { movement : MouseMovement
    , isLocked : Bool
    }

{-| When the application first starts, this is the initial state of the Model.
Not using the movement attribute of Args at this time;
it's a carryover from the original, and the additional complexity
to actually use it is probably not worth it in this case.
It's still a useful example using Html.programWithFlags though.
-}
init : (worldModel, Cmd worldMsg) -> Args -> (Model worldModel, Cmd (Msg worldMsg))
init worldInit { movement, isLocked } =
    let (worldModel, worldCmdMsg) = worldInit in
    ( { numPlayers = 1
      , person = defaultPerson
      , player2 = defaultPerson
      , globalTime = 0
      , maybeWindowSize = Nothing
      , keys = Keys False False False False False
      , gamepadIds = []
      , inputs = noInput
      , inputs2 = noInput
      , wantToBeLocked = True
      , isLocked = isLocked
      , message = "No texture yet"
      , worldModel = worldModel
      }
    , Cmd.batch
        [ Window.size |> Task.perform (always Resize (0, 0)) Resize
        , gamepads GamepadUpdate
        , Cmd.map WorldMessage worldCmdMsg
        ]
    )

{-
direction : Person -> Vec3
direction person =
    let h = person.horizontalAngle
        v = person.verticalAngle
    in
        vec3 (cos h) (sin v) (sin h)
-}

{-
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (makeRotate, transform)

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

-}

orient : Person -> Vec3 -> Vec3
orient person = Orientation.rotateBodyV person.orientation

direction : Person -> Vec3
direction person = orient person V3.k

cameraUp : Person -> Vec3
-- cameraUp person = orient person V3.j
-- cameraUp person = Qn.vrotate (Qn.negate person.orientQn) V3.j
cameraUp person = Orientation.rotateLabV person.orientation V3.j
