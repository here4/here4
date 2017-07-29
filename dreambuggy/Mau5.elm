module Mau5 exposing (create, Flags, Model, Msg)

import Control exposing (..)
import Dispatch exposing (..)
import Model
import Ports
import World exposing (..)
import App exposing (..)


type alias Flags =
    { movement : MouseMovement
    , isLocked : Bool
    }

{-| This type is returned by the fullscreen JS api in Mau5.js
for mouse movement
-}
type alias MouseMovement =
    ( Int, Int )


type Mau5Msg
    = MouseMove MouseMovement
    | LockRequest Bool
    | LockUpdate Bool


type alias Msg =
    WorldMsg Mau5Msg


type alias Mau5Model =
    { wantToBeLocked : Bool
    , isLocked : Bool
    }


type alias Model =
    Multiverse Mau5Model


create :
    List World.Attributes
    -> Program Flags (Model.Model Model Msg) (Model.Msg navMsg Msg)
create attributes =
    World.create init update attributes


init : Flags -> ( Mau5Model, Cmd Mau5Msg )
init flags =
    ( { wantToBeLocked = True
      , isLocked = flags.isLocked
      }
    , Cmd.none )

update : Mau5Msg -> Mau5Model -> ( Mau5Model, Cmd Mau5Msg )
update msg model =
    case msg of
        MouseMove movement ->
            -- ( { model | inputs = mouseToInputs movement model.inputs }, Cmd.none )
            ( model, Cmd.none )

        LockRequest wantToBeLocked ->
            ( { model | wantToBeLocked = wantToBeLocked }
            , if model.wantToBeLocked == model.isLocked then
                Cmd.none
              else if model.wantToBeLocked then
                Ports.requestPointerLock ()
              else
                Ports.exitPointerLock ()
            )

        LockUpdate isLocked ->
            ( { model | isLocked = isLocked }, Cmd.none )


-- subscriptions : Mau5Model -> Sub Mau5Msg


mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs ( mx, my ) inputs =
    { inputs | mx = 0.5 * inputs.dt * toFloat mx, my = -0.5 * inputs.dt * toFloat my }
