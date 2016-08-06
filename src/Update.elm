module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (makeRotate, transform)
import Time exposing (Time)

import Bag
import Dispatch exposing (..)
import Model exposing (Model, Msg, WorldCtrl)
import Model
import Orientation
import Ports

import Gamepad
import GamepadInputs

import Things.Terrain as Terrain
import Things.Terrain exposing (Terrain)
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.LookAt as LookAt
import Vehicles.DreamDebug as DreamDebug

{-| Take a Msg and a Model and return an updated Model
-}
update : (Dispatch WorldCtrl worldMsg -> worldModel -> (worldModel, Cmd (Dispatch WorldCtrl worldMsg)))
    -> (worldModel -> Maybe Bag.Key)
    -> (worldModel -> Maybe Terrain)
    -> (Time -> worldModel -> worldModel)
    -> Model.Msg (Dispatch WorldCtrl worldMsg) -> Model worldModel
    -> (Model worldModel, Cmd (Msg (Dispatch WorldCtrl worldMsg)))
update worldUpdate worldAnything  worldTerrain worldAnimate msg model =
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
            let (model', newCmdMsg) = case worldTerrain model.worldModel of
                Nothing -> (model, Cmd.none)
                Just terrain ->
                    let inputs = timeToInputs dt model.inputs
                        inputs2 = timeToInputs dt model.inputs2
                        wm = worldAnimate inputs.dt model.worldModel
                        (wm2, wmCmdMsg) = case worldAnything model.worldModel of
                            Nothing -> (wm, Cmd.none)
                            Just focKey ->
                                let dp = vec3 inputs.cx inputs.cy 0
                                in worldUpdate (Down (Model.Move focKey dp)) wm
                        focPos = vec3 -2 20 -17
                        newModel =
                            { model | globalTime = model.globalTime + dt
                                    , person = step terrain inputs (Just focPos) model.person
                                    , player2 = step terrain inputs2 Nothing model.player2
                                    , inputs = clearStationaryInputs inputs
                                    , worldModel = wm2
                            }
                    in
                        (newModel, Cmd.map Model.WorldMessage wmCmdMsg)
            in ( model'
               , Cmd.batch
                   [ Gamepad.gamepads Model.GamepadUpdate
                   , newCmdMsg
                   ]
               )

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

aboveTerrain : Model.EyeLevel -> Vec3 -> Vec3
aboveTerrain eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

step : Terrain -> Model.Inputs -> Maybe Vec3 -> Model.Person -> Model.Person
step terrain inputs focPos person0 = if inputs.reset then Model.defaultPerson else
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
            move person =
                if person.vehicle == Model.vehicleBird then
                    DreamBird.move eyeLevel inputs person
                else if person.vehicle == Model.vehicleBuggy then
                    DreamBuggy.move eyeLevel inputs person
                else if person.vehicle == Model.vehicleLookAt then
                    LookAt.move eyeLevel inputs focPos person
                else
                    DreamDebug.move eyeLevel inputs person
            bounds person = { person | pos = Terrain.bounds terrain person.pos }

            checkCamera person = { person |
                cameraInside = if inputs.changeCamera then
                                   not person.cameraInside
                               else
                                   person.cameraInside,
                cameraVR = if inputs.changeVR then
                                   not person.cameraVR
                               else
                                   person.cameraVR }

            moveCamera person =
                if person.cameraInside then
                    -- let behind = person.pos `sub` (V3.scale 2.5 (Model.direction person)) `sub` (vec3 0 0.5 0)
                    let inside = person.pos
                                     `add` Orientation.rotateBodyV person.orientation (vec3 0 0 1) -- wedge
                                     -- Inside Jeep driver's seat
                                     -- `add` Qn.vrotate person.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        { person | cameraPos = inside -- aboveTerrain eyeLevel behind
                                 , cameraUp = Model.cameraUp person }
                else
                    let behind = person.pos `sub` (V3.scale 7 (Model.direction person))
                        p = toRecord person.pos
                        yMax0 v = let vr = V3.toRecord v in vec3 vr.x (min (-0.3) vr.y) vr.z
                        newCameraPos =
                            if p.y < Model.eyeLevel then
                                yMax0 (vec3 0 2 0 `add` behind)
                            else if p.y < Model.eyeLevel+1 then
                                behind
                            else
                                -- vec3 0 2 0 `add` behind
                                vec3 0 -2 0 `add` behind
                        cameraPos = aboveTerrain eyeLevel
                            -- (V3.scale 0.5 newCameraPos `add` V3.scale 0.5 person.cameraPos) -- smooth
                            newCameraPos
                        newCameraUp = Model.cameraUp person

                    in  { person | cameraPos = Terrain.bounds terrain cameraPos
                                 , cameraUp =
                            -- V3.scale 0.1 newCameraUp `add` V3.scale 0.9 person.cameraUp }
                               newCameraUp }
        in
            person0
                |> gravity eyeLevel inputs.dt
                |> selectVehicle inputs
                |> move
                |> bounds
                |> checkCamera
                |> moveCamera

selectVehicle : Model.Inputs -> Model.Person -> Model.Person
selectVehicle inputs person =
    let
        switch = inputs.button_X
        newVehicle = Model.nextVehicle person.vehicle
    in
        if not switch then
            person
        else if newVehicle == Model.vehicleBuggy then
            Debug.log "Switch to buggy!" <|
                DreamBuggy.welcome { person | vehicle = newVehicle }
        else if newVehicle == Model.vehicleBird then
            Debug.log "Switch to flying!" <|
                DreamBird.welcome { person | vehicle = newVehicle }
        else if newVehicle == Model.vehicleLookAt then
            Debug.log "Switch to LookAt!" <|
                LookAt.welcome { person | vehicle = newVehicle }
        -- else if newVehicle == vehicleDebug then
        else
            Debug.log "Switch to debug!" <|
                DreamDebug.welcome { person | vehicle = newVehicle }

gravity : Model.EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
