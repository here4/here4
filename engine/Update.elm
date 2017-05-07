module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Time exposing (Time)

import Dispatch exposing (..)
import Model exposing (Model, Msg)
import Orientation
import Ports

import Gamepad
import GamepadInputs

import Control exposing (WorldMsg)
import App exposing (Focus)
import Ground exposing (Ground, bounds, elevation)
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.LookAt as LookAt
import Vehicles.DreamDebug as DreamDebug

{-| Take a Msg and a Model and return an updated Model
-}
update : (WorldMsg worldMsg -> worldModel -> (worldModel, Cmd (WorldMsg worldMsg)))
    -> (worldModel -> Maybe Focus)
    -> (worldModel -> Maybe Ground)
    -> (Time -> worldModel -> worldModel)
    -> Model.Msg (WorldMsg worldMsg) -> Model worldModel
    -> (Model worldModel, Cmd (Msg (WorldMsg worldMsg)))
update worldUpdate worldFocus worldTerrain worldAnimate msg model =
    case msg of
        Model.WorldMessage worldMsg ->
            let (worldModel, worldCmdMsg) = worldUpdate worldMsg model.worldModel in
            ( { model | worldModel = worldModel }, Cmd.map Model.WorldMessage worldCmdMsg )
        Model.KeyChange keyfunc ->
            let keys = keyfunc model.keys in
            ( { model | keys = keys
                      , inputs = keysToInputs keys model.inputs }
            , Cmd.none )
        Model.Resize windowSize ->
            ( { model | maybeWindowSize = Just windowSize }, Cmd.none )
        Model.MouseMove movement ->
            ( { model | inputs = mouseToInputs movement model.inputs }, Cmd.none )
        Model.GamepadUpdate gps0 ->
            ( updateGamepads gps0 model, Cmd.none )
        Model.LockRequest wantToBeLocked ->
            ( { model | wantToBeLocked = wantToBeLocked }
            , if model.wantToBeLocked == model.isLocked then
                Cmd.none
              else if model.wantToBeLocked then
                Ports.requestPointerLock ()
              else
                Ports.exitPointerLock ()
            ) 
        Model.LockUpdate isLocked ->
            ( { model | isLocked = isLocked }, Cmd.none )
        Model.Animate dt ->
            let (model_, newCmdMsg) = case worldTerrain model.worldModel of
                Nothing -> (model, Cmd.none)
                Just terrain ->
                    let inputs = timeToInputs dt model.inputs
                        inputs2 = timeToInputs dt model.inputs2
                        wm = worldAnimate inputs.dt model.worldModel
                        (wm2, wmCmdMsg, focPos) = case worldFocus model.worldModel of
                            Just focus ->
                                let dp = inputsToMove inputs model.player1
                                    (wm2, wmCmdMsg) = worldUpdate (Forward (Control.Move dp)) wm
                                in (wm2, wmCmdMsg, Just focus.pos)
                            _ -> (wm, Cmd.none, Nothing)
                        newModel =
                            { model | globalTime = model.globalTime + dt
                                    , player1 = step terrain inputs focPos model.player1
                                    , player2 = step terrain inputs2 Nothing model.player2
                                    , inputs = clearStationaryInputs inputs
                                    , worldModel = wm2
                            }
                    in
                        (newModel, Cmd.map Model.WorldMessage wmCmdMsg)
            in ( model_
               , Cmd.batch
                   [ Gamepad.gamepads Model.GamepadUpdate
                   , newCmdMsg
                   ]
               )

inputsToMove : Model.Inputs -> Model.Player -> Vec3
inputsToMove inputs player =
    let dp = vec3 -inputs.cx 0 inputs.cy
    in Orientation.rotateBodyV player.orientation dp

timeToInputs : Time -> Model.Inputs -> Model.Inputs
timeToInputs dt inputs0 = { inputs0 | dt = dt }

keysToInputs : Model.Keys -> Model.Inputs -> Model.Inputs
keysToInputs keys inputs0 =
    let minusPlus a b = if a && not b then -1 else if b && not a then 1 else 0
    in
        { inputs0 | x = minusPlus keys.left keys.right
                  , y = minusPlus keys.down keys.up
                  , button_X = keys.space
        }

mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs (mx,my) inputs0 = { inputs0 | mx = toFloat mx / 500, my = toFloat my / 500 }

clearStationaryInputs : Model.Inputs -> Model.Inputs
clearStationaryInputs inputs0 = { inputs0 | mx = 0, my = 0 }

gamepadToInputs : Gamepad.Gamepad -> Model.Inputs -> Model.Inputs
gamepadToInputs gamepad0 inputs0 =
    let gamepad = GamepadInputs.toStandardGamepad gamepad0
        {x,y,mx,my,cx,cy} = GamepadInputs.gamepadToArrows gamepad
        bs = GamepadInputs.gamepadToButtons gamepad
        risingEdge old new = new && (not old)
    in  { inputs0 | reset = bs.bStart
                  , changeVR = risingEdge inputs0.changeVR bs.bB
                  , changeCamera = risingEdge inputs0.changeCamera bs.bRightBumper
                  , x = x, y = y
                  , mx=mx, my=my
                  , cx=cx, cy=cy
                  , button_X = risingEdge inputs0.button_X bs.bX }

updateGamepads : List Gamepad.Gamepad -> Model worldModel -> Model worldModel
updateGamepads gps0 model =
    let (gps, is) = GamepadInputs.persistentGamepads model.gamepadIds gps0 in
    case gps of
      [] -> model
      [Just gp] ->
        { model | numPlayers = 1
                , inputs = gamepadToInputs gp model.inputs
                , gamepadIds = is
        }
      (Just gp :: Nothing :: _) ->
        { model | numPlayers = 1
                , inputs = gamepadToInputs gp model.inputs
                , gamepadIds = is
        }
      (Nothing :: Just gp2 :: _) ->
        { model | numPlayers = 2
                , inputs2 = gamepadToInputs gp2 model.inputs2
                , gamepadIds = is
        }
      (Just gp :: Just gp2 :: _) ->
        { model | numPlayers = 2
                , inputs = gamepadToInputs gp model.inputs
                , inputs2 = gamepadToInputs gp2 model.inputs2
                , gamepadIds = is
        }
      _ -> model

aboveGround : Model.EyeLevel -> Vec3 -> Vec3
aboveGround eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

step : Ground -> Model.Inputs -> Maybe Vec3 -> Model.Player -> Model.Player
step terrain inputs focPos player0 = if inputs.reset then Model.defaultPlayer else
        let 
            eyeLevel pos = Model.eyeLevel + elevation terrain pos
            move player =
                if player.vehicle == Model.vehicleBird then
                    DreamBird.move eyeLevel inputs player
                else if player.vehicle == Model.vehicleBuggy then
                    DreamBuggy.move eyeLevel inputs player
                else if player.vehicle == Model.vehicleLookAt then
                    LookAt.move eyeLevel inputs focPos player
                else
                    DreamDebug.move eyeLevel inputs player
            keepWithinbounds player = { player | pos = bounds terrain player.pos }

            checkCamera player = { player |
                cameraInside = if inputs.changeCamera then
                                   not player.cameraInside
                               else
                                   player.cameraInside,
                cameraVR = if inputs.changeVR then
                                   not player.cameraVR
                               else
                                   player.cameraVR }

            moveCamera player =
                if player.cameraInside then
                    -- let behind = player.pos `sub` (V3.scale 2.5 (Model.direction player)) `sub` (vec3 0 0.5 0)
                    let inside = add player.pos
                                     (Orientation.rotateBodyV player.orientation (vec3 0 0 1)) -- wedge
                                     -- Inside Jeep driver's seat
                                     -- `add` Qn.vrotate player.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        { player | cameraPos = inside -- aboveGround eyeLevel behind
                                 , cameraUp = Model.cameraUp player }
                else
                    let behind = sub player.pos (V3.scale 7 (Model.direction player))
                        p = toRecord player.pos
                        yMax0 v = let vr = V3.toRecord v in vec3 vr.x (min (-0.3) vr.y) vr.z
                        newCameraPos =
                            if p.y < Model.eyeLevel then
                                yMax0 (add (vec3 0 2 0) behind)
                            else if p.y < Model.eyeLevel+1 then
                                behind
                            else
                                -- add (vec3 0 2 0) behind
                                add (vec3 0 -2 0) behind
                        cameraPos = aboveGround eyeLevel
                            -- (V3.scale 0.5 newCameraPos `add` V3.scale 0.5 player.cameraPos) -- smooth
                            newCameraPos
                        newCameraUp = Model.cameraUp player

                    in  { player | cameraPos = bounds terrain cameraPos
                                 , cameraUp =
                            -- V3.scale 0.1 newCameraUp `add` V3.scale 0.9 player.cameraUp }
                               newCameraUp }
        in
            player0
                |> gravity eyeLevel inputs.dt
                |> selectVehicle inputs
                |> move
                |> keepWithinbounds
                |> checkCamera
                |> moveCamera

selectVehicle : Model.Inputs -> Model.Player -> Model.Player
selectVehicle inputs player =
    let
        switch = inputs.button_X
        newVehicle = Model.nextVehicle player.vehicle
    in
        if not switch then
            player
        else if newVehicle == Model.vehicleBuggy then
            Debug.log "Switch to buggy!" <|
                DreamBuggy.welcome { player | vehicle = newVehicle }
        else if newVehicle == Model.vehicleBird then
            Debug.log "Switch to flying!" <|
                DreamBird.welcome { player | vehicle = newVehicle }
        else if newVehicle == Model.vehicleLookAt then
            Debug.log "Switch to LookAt!" <|
                LookAt.welcome { player | vehicle = newVehicle }
        -- else if newVehicle == vehicleDebug then
        else
            Debug.log "Switch to debug!" <|
                DreamDebug.welcome { player | vehicle = newVehicle }

gravity : Model.EyeLevel -> Float -> Model.Player -> Model.Player
gravity eyeLevel dt player =
  if getY player.pos <= eyeLevel player.pos then player else
    let v = toRecord player.velocity
    in
        { player | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
