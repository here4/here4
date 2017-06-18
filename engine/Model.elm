module Model exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Time exposing (..)
import Task exposing (Task)
import Window
import Orientation exposing (Orientation)
import Bag
import Body exposing (Body, Oriented)
import Camera exposing (Camera, Shot)
import Camera.Util exposing (toCamera)
import Ground exposing (Ground)
import Gamepad exposing (Gamepad, gamepads)


type Msg worldMsg
    = KeyChange (Keys -> Keys)
    | MouseMove MouseMovement
    | GamepadUpdate (List Gamepad)
    | LockRequest Bool
    | LockUpdate Bool
    | Animate Time
    | Resize Window.Size
    | WorldMessage worldMsg


type alias World =
    { bodies : List Body
    , ground : Ground
    }


type alias Motion =
    { position : Vec3
    , velocity : Vec3
    , orientation : Orientation
    }

type alias Player =
    { camera : Camera
    , shot : Maybe Shot
    , rideKey : Maybe Bag.Key
    , rideLabel : String
    , focusKey : Bag.Key
    , cameraVR : Bool
    , overlay : Bool
    }


type Eye
    = OneEye
    | LeftEye
    | RightEye


type alias EyeLevel =
    Vec3 -> Float


eyeLevel : Float
eyeLevel =
    1.8


defaultMotion : Motion
defaultMotion =
    { position = vec3 0 30 0
    , velocity = vec3 0 0 0
    , orientation = Orientation.initial
    }

defaultCamera : Camera
defaultCamera = toCamera
    { position = vec3 0 eyeLevel 0
    , orientation = Orientation.initial
    }

defaultPlayer : Player
defaultPlayer =
    { camera = defaultCamera
    , shot = Nothing
    , rideKey = Nothing
    , rideLabel = ""
    , focusKey = 0
    , cameraVR = False
    , overlay = False
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , tab : Bool
    , shift : Bool
    , space : Bool
    , kW : Bool
    , kA : Bool
    , kS : Bool
    , kD : Bool
    , kH : Bool
    , kJ : Bool
    , kK : Bool
    , kL : Bool
    , kI : Bool
    , kC : Bool
    , kP : Bool
    , kComma : Bool
    , kPeriod : Bool
    }

noKeys : Keys
noKeys =
    { left = False
    , right = False
    , up = False
    , down = False
    , tab = False
    , shift = False
    , space = False
    , kW = False
    , kA = False
    , kS = False
    , kD = False
    , kH = False
    , kJ = False
    , kK = False
    , kL = False
    , kI = False
    , kC = False
    , kP = False
    , kComma = False
    , kPeriod = False
    }


type alias Inputs =
    { reset : Bool
    , changeVR : Bool
    , prevCamera : Bool
    , nextCamera : Bool
    , nextOverlay : Bool
    , isJumping : Bool
    , button_X : Bool
    , rightTrigger : Float
    , leftTrigger : Float
    , x : Float
    , y : Float
    , mx : Float
    , my : Float
    , cx : Float
    , cy : Float
    , dt : Float
    }


noInput : Inputs
noInput =
    { reset = False
    , changeVR = False
    , prevCamera = False
    , nextCamera = False
    , nextOverlay = False
    , isJumping = False
    , button_X = False
    , rightTrigger = 0.0
    , leftTrigger = 0.0
    , x = 0
    , y = 0
    , mx = 0
    , my = 0
    , cx = 0
    , cy = 0
    , dt = 0
    }


{-| This type is returned by the fullscreen JS api in PointerLock.js
for mouse movement
-}
type alias MouseMovement =
    ( Int, Int )


{-| This is the application's Model data structure
-}
type alias Model worldModel =
    { numPlayers : Int
    , player1 : Player
    , player2 : Player
    , globalTime : Time
    , paused : Bool
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
init : ( worldModel, Cmd worldMsg ) -> Args -> ( Model worldModel, Cmd (Msg worldMsg) )
init worldInit { movement, isLocked } =
    let
        ( worldModel, worldCmdMsg ) =
            worldInit
    in
        ( { numPlayers = 1
          , player1 = defaultPlayer
          , player2 = defaultPlayer
          , globalTime = 0
          , paused = False
          , maybeWindowSize = Nothing
          , keys = noKeys
          , gamepadIds = []
          , inputs = noInput
          , inputs2 = noInput
          , wantToBeLocked = True
          , isLocked = isLocked
          , message = "No texture yet"
          , worldModel = worldModel
          }
        , Cmd.batch
            -- [ Window.size |> Task.perform (always Resize (0, 0)) Resize
            [ Window.size |> Task.perform Resize
            , gamepads GamepadUpdate
            , Cmd.map WorldMessage worldCmdMsg
            ]
        )


orient : Motion -> Vec3 -> Vec3
orient motion =
    Orientation.rotateBodyV motion.orientation


direction : Oriented a -> Vec3
direction motion =
    Orientation.rotateBodyV motion.orientation V3.k


