module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Maybe.Extra exposing (isJust)
import Time exposing (Time)

import Bag
import Dispatch exposing (..)
import Model exposing (Model, Msg)
import Orientation exposing (fromVec3)
-- import Quaternion as Qn -- Don't expose this here
import Ports

import Gamepad
import GamepadInputs

import Control exposing (WorldMsg)
import Body exposing (Camera)
import App exposing (Focus)
import Ground exposing (Ground)
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.LookAt as LookAt
import Vehicles.DreamDebug as DreamDebug

{-| Take a Msg and a Model and return an updated Model
-}
update : (WorldMsg worldMsg -> worldModel -> (worldModel, Cmd (WorldMsg worldMsg)))
    -> (Maybe Bag.Key -> worldModel -> String)
    -> (worldModel -> Int)
    -> (worldModel -> Maybe Ground)
    -> (Ground -> Time -> worldModel -> worldModel)
    -> (Maybe Bag.Key -> worldModel -> Maybe Camera)
    -> (Bag.Key -> worldModel -> Maybe Focus)
    -> Model.Msg (WorldMsg worldMsg) -> Model worldModel
    -> (Model worldModel, Cmd (Msg (WorldMsg worldMsg)))
update worldUpdate worldLabel worldKeyLimit worldTerrain worldAnimate worldCamera worldFocus msg model =
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
                    let inputs1 = timeToInputs dt model.inputs
                        inputs2 = timeToInputs dt model.inputs2
                        keyLimit = worldKeyLimit model.worldModel

                        -- Animate
                        wm = worldAnimate terrain inputs1.dt model.worldModel

                        -- Change ride?
                        hasCamera key = isJust (worldCamera (Just key) model.worldModel)
                        player1 = selectVehicle hasCamera keyLimit inputs1 model.player1
                        player2 = selectVehicle hasCamera keyLimit inputs2 model.player2

                        -- Camera
                        label1 = worldLabel (player1.rideKey) model.worldModel
                        camera1 = worldCamera (player1.rideKey) model.worldModel
                        (wm1, wm1Msg) = case player1.rideKey of
                            Just key -> worldUpdate (Forward key (Control.Drive terrain inputs1)) wm
                            Nothing  -> (wm, Cmd.none)

                        label2 = worldLabel (player2.rideKey) model.worldModel
                        camera2 = worldCamera (player2.rideKey) model.worldModel
                        (wm2, wm2Msg) = case player2.rideKey of
                            Just key -> worldUpdate (Forward key (Control.Drive terrain inputs2)) wm1
                            Nothing  -> (wm1, Cmd.none)

                        -- Focus
                        (wmF, wmFMsg, focPos) = let key = player1.focusKey in
                            case worldFocus key model.worldModel of
                                Just focus ->
                                    let dp = inputsToMove inputs1 player1
                                        (wmF, wmFMsg) =
                                            worldUpdate (Forward key (Control.Move dp)) wm2
                                    in (wmF, wmFMsg, Just focus.position)
                                _ -> (wm2, Cmd.none, Nothing)

                        newModel =
                            { model | globalTime = model.globalTime + dt
                                    , player1 = step terrain inputs1 label1 camera1 focPos player1
                                    , player2 = step terrain inputs2 label2 camera2 Nothing player2
                                    , inputs = clearStationaryInputs inputs1
                                    , worldModel = wmF
                            }
                    in -- Cmd.batch ]wm1Msg, wm2Msg, wmFMsg]
                        (newModel, Cmd.map Model.WorldMessage wmFMsg)
            in ( model_
               , Cmd.batch
                   [ Gamepad.gamepads Model.GamepadUpdate
                   , newCmdMsg
                   ]
               )

inputsToMove : Model.Inputs -> Model.Player -> Vec3
inputsToMove inputs player =
    let dp = vec3 -inputs.cx 0 inputs.cy
    in Orientation.rotateBodyV player.motion.orientation dp

timeToInputs : Time -> Model.Inputs -> Model.Inputs
timeToInputs dt inputs0 = { inputs0 | dt = dt }

keysToInputs : Model.Keys -> Model.Inputs -> Model.Inputs
keysToInputs keys inputs0 =
    let minusPlus a b = if a && not b then -1 else if b && not a then 1 else 0
    in
        { inputs0 | x = minusPlus keys.left keys.right
                  , y = minusPlus keys.down keys.up
                  , button_X = keys.space
                  , changeCamera = keys.kI
                  , mx = minusPlus keys.kComma keys.kPeriod
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

step : Ground -> Model.Inputs -> String -> Maybe Camera -> Maybe Vec3 -> Model.Player -> Model.Player
step terrain inputs label camera focPos player0 = if inputs.reset then Model.defaultPlayer else
        let 
            eyeLevel pos = Model.eyeLevel + terrain.elevation pos

            move player = case camera of
                Just c  -> { player | motion = { position = c.position,
                                                 velocity = player.motion.velocity,
                                                 orientation = fromVec3 c.orientation
                                               }
                           }
                Nothing -> player

            relabel player = { player | rideLabel = label }

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
                    -- let behind = player.pos `sub` (V3.scale 2.5 (Model.direction player.motion)) `sub` (vec3 0 0.5 0)
                    let inside = add player.motion.position
                                     (Orientation.rotateBodyV player.motion.orientation (vec3 0 0 3)) -- wedge
                                     -- Inside Jeep driver's seat
                                     -- `add` Qn.vrotate player.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        { player | cameraPos = inside -- aboveGround eyeLevel behind
                                 , cameraUp = Model.cameraUp player }
                else
                    let behind = sub player.motion.position (V3.scale 20 (Model.direction player.motion))
                        p = toRecord player.motion.position
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

                    in  { player | cameraPos = terrain.bounds cameraPos
                                 , cameraUp =
                            -- V3.scale 0.1 newCameraUp `add` V3.scale 0.9 player.cameraUp }
                               newCameraUp }
        in
            player0
                |> relabel
                |> move
                |> checkCamera
                |> moveCamera

selectVehicle : (Bag.Key -> Bool) -> Bag.Key -> Model.Inputs -> Model.Player -> Model.Player
selectVehicle hasCamera keyLimit inputs player =
    let nextKey key = (key+1) % keyLimit

        findCameraHelp origKey key =
            if hasCamera key then
                key
            else
                let next = nextKey key
                in
                    if next == origKey then
                        origKey
                    else
                        findCameraHelp origKey next

        findCamera key = findCameraHelp key key

        key = findCamera (Maybe.withDefault 0 player.rideKey)
    in
        if inputs.button_X then
            { player | rideKey = Just (findCamera (nextKey key)) }
        else
            { player | rideKey = Just key }
