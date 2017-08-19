module Here4.Model exposing (..)

import Color exposing (Color)
import Gamepad exposing (Gamepad, gamepads)
import Here4.Bag as Bag
import Here4.Body exposing (Body, Oriented)
import Here4.Camera.Types exposing (Camera, Shot)
import Here4.Camera exposing (defaultCamera)
import Here4.Ground exposing (Ground)
import Here4.Orientation as Orientation exposing (Orientation)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Task exposing (Task)
import Time exposing (..)
import Window


type WorldKey a
    = WorldKey Bag.Key a


type AppKey
    = AppKey Bag.Key


type PartyKey
    = PartyKey Bag.Key


type PlayerKey
    = PlayerKey Bag.Key


type Msg worldMsg
    = KeyChange (Keys -> Keys)
    | GamepadUpdate (List Gamepad)
    | JoinWorld (WorldKey ()) PlayerKey -- should these be multiworld hub messages?
    | LeaveWorld (WorldKey PlayerKey) -- should these be multiworld hub messages?
    | Animate Time
    | Resize Window.Size
    | WorldMessage worldMsg
    | WorldEffect GlobalMsg
    | NavigatorEffect NavigatorMsg


type NavigatorMsg
    = ProvideInputs Inputs

type GlobalMsg
    = PlayerUpdate (WorldKey PartyKey) (WorldKey PartyKey)


type alias World =
    { backgroundColor : Color
    , bodies : Vec3 -> List Body
    , ground : Ground
    }


type alias Motion =
    { position : Vec3
    , velocity : Vec3
    , orientation : Orientation
    }


type alias Player msg =
    { partyKey : Maybe (WorldKey PartyKey)
    , rideLabel : String
    , focusKey : WorldKey AppKey
    , camera : Camera -- smoothed, current view camera
    , rawCamera : Camera
    , recentRawCameras : List Camera
    , shot : Maybe Shot
    , cameraVR : Bool
    , overlayVisible : Bool
    , overlayContent : Html msg
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


defaultPlayer : Player msg
defaultPlayer =
    { partyKey = Nothing
    , camera = defaultCamera
    , rawCamera = defaultCamera
    , recentRawCameras = []
    , shot = Nothing
    , rideLabel = ""
    , focusKey = WorldKey 0 (AppKey 0)
    , cameraVR = False
    , overlayVisible = False
    , overlayContent = Html.text ""
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , tab : Bool
    , shift : Bool
    , space : Bool
    , pageUp : Bool
    , pageDown : Bool
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
    , pageUp = False
    , pageDown = False
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
    , toggleOverlay : Bool
    , prevOverlay : Bool
    , nextOverlay : Bool
    , button_X : Bool
    , button_Y : Bool
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
    , toggleOverlay = False
    , prevOverlay = False
    , nextOverlay = False
    , button_X = False
    , button_Y = False
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
type alias Model worldModel worldMsg =
    { numPlayers : Int
    , player1 : Player worldMsg
    , player2 : Player worldMsg
    , globalTime : Time
    , paused : Bool
    , maybeWindowSize : Maybe Window.Size
    , keys : Keys
    , gamepadIds : List String
    , inputs : Inputs
    , inputs2 : Inputs
    , message : String
    , multiverse : worldModel
    }


playerJoin : PlayerKey -> Cmd (Msg worldMsg)
playerJoin playerKey =
    Task.succeed playerKey |> Task.perform (JoinWorld (WorldKey 0 ()))


{-| When the application first starts, this is the initial state of the Model.
-}
init :
    (flags -> ( worldModel, Cmd worldMsg ))
    -> flags
    -> ( Model worldModel worldMsg, Cmd (Msg worldMsg) )
init worldInit flags =
    let
        ( worldModel, worldCmdMsg ) =
            worldInit flags
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
          , message = "No texture yet"
          , multiverse = worldModel
          }
        , Cmd.batch
            [ Window.size |> Task.perform Resize
            , gamepads GamepadUpdate
            , playerJoin (PlayerKey 0)

            -- , playerJoin (PlayerKey 1)
            , Cmd.map WorldMessage worldCmdMsg
            ]
        )


orient : Motion -> Vec3 -> Vec3
orient motion =
    Orientation.rotateBodyV motion.orientation


direction : Oriented a -> Vec3
direction motion =
    Orientation.rotateBodyV motion.orientation V3.k
