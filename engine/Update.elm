module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Maybe.Extra exposing (isJust)
import Time exposing (Time)

import App exposing (Focus)
import Bag
import Body exposing (reposition)
import Camera exposing (..)
import Camera.POV exposing (pov)
import Camera.DollyArc exposing (dolly)
import Camera.Tracking exposing (tracking)
import Camera.Util as Camera
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Gamepad
import GamepadInputs
import Ground exposing (Ground)
import Model exposing (Model, Msg)
import Orientation exposing (fromVec3)
import Ports


{-| Take a Msg and a Model and return an updated Model
-}
update :
    (WorldMsg worldMsg -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) ))
    -> (Maybe Bag.Key -> worldModel -> String)
    -> (worldModel -> Int)
    -> (worldModel -> Maybe Ground)
    -> (Ground -> Time -> worldModel -> worldModel)
    -> (Maybe Bag.Key -> worldModel -> Maybe Framing)
    -> (Bag.Key -> worldModel -> Maybe Focus)
    -> Model.Msg (WorldMsg worldMsg)
    -> Model worldModel
    -> ( Model worldModel, Cmd (Msg (WorldMsg worldMsg)) )
update worldUpdate worldLabel worldKeyLimit worldTerrain worldAnimate worldFraming worldFocus msg model =
    case msg of
        Model.WorldMessage worldMsg ->
            let
                ( worldModel, worldCmdMsg ) =
                    worldUpdate worldMsg model.worldModel
            in
                ( { model | worldModel = worldModel }, Cmd.map Model.WorldMessage worldCmdMsg )

        Model.KeyChange keyfunc ->
            let
                keys =
                    keyfunc model.keys
            in
                ( { model
                    | keys = keys
                    , inputs = keysToInputs keys model.inputs
                  }
                , Cmd.none
                )

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
            let
                ( model_, newCmdMsg ) =
                    case worldTerrain model.worldModel of
                        Nothing ->
                            ( model, Cmd.none )

                        Just terrain ->
                            let
                                inputs1 =
                                    timeToInputs dt model.inputs

                                inputs2 =
                                    timeToInputs dt model.inputs2

                                keyLimit =
                                    worldKeyLimit model.worldModel

                                -- Animate
                                wm =
                                    worldAnimate terrain inputs1.dt model.worldModel

                                -- Change ride?
                                hasFraming key =
                                    isJust (worldFraming (Just key) wm)

                                player1 =
                                    selectCamera terrain hasFraming keyLimit inputs1 model.player1

                                player2 =
                                    selectCamera terrain hasFraming keyLimit inputs2 model.player2

                                label1 =
                                    worldLabel (player1.rideKey) wm

                                label2 =
                                    worldLabel (player2.rideKey) wm

                                ( wm1, wm1Msg ) =
                                    case player1.rideKey of
                                        Just key ->
                                            worldUpdate (Forward key (Control.Drive terrain inputs1)) wm

                                        Nothing ->
                                            ( wm, Cmd.none )

                                ( wm2, wm2Msg ) =
                                    case player2.rideKey of
                                        Just key ->
                                            worldUpdate (Forward key (Control.Drive terrain inputs2)) wm1

                                        Nothing ->
                                            ( wm1, Cmd.none )

                                -- Focus
                                ( wmF, wmFMsg, focPos ) =
                                    let
                                        key =
                                            player1.focusKey
                                    in
                                        case worldFocus key model.worldModel of
                                            Just focus ->
                                                let
                                                    dp =
                                                        inputsToMove inputs1 player1

                                                    ( wmF, wmFMsg ) =
                                                        worldUpdate (Forward key (Control.Move dp)) wm2
                                                in
                                                    ( wmF, wmFMsg, Just focus.position )

                                            _ ->
                                                ( wm2, Cmd.none, Nothing )

                                -- Camera
                                framing1 =
                                    worldFraming player1.rideKey wmF

                                framing2 =
                                    worldFraming player2.rideKey wmF

                                newModel =
                                    { model
                                        | globalTime = model.globalTime + dt
                                        , player1 = updatePlayer terrain inputs1 label1 player1.shot framing1 player1
                                        , player2 = updatePlayer terrain inputs2 label2 player2.shot framing2 player2
                                        , inputs = clearStationaryInputs inputs1
                                        , worldModel = wmF
                                    }

                                gamepadUpdateMsg = Gamepad.gamepads Model.GamepadUpdate
                                wMsg = Cmd.map Model.WorldMessage wmFMsg
                            in
                                ( newModel, Cmd.batch [ gamepadUpdateMsg, wMsg ] )
            in
                ( model_ , newCmdMsg)


inputsToMove : Model.Inputs -> Model.Player -> Vec3
inputsToMove inputs player =
    let
        dp =
            vec3 -inputs.cx 0 inputs.cy
    in
        Orientation.rotateBodyV player.camera.orientation dp


timeToInputs : Time -> Model.Inputs -> Model.Inputs
timeToInputs dt inputs0 =
    { inputs0 | dt = dt }


keysToInputs : Model.Keys -> Model.Inputs -> Model.Inputs
keysToInputs keys inputs0 =
    let
        minusPlus a b =
            if a && not b then
                -1
            else if b && not a then
                1
            else
                0
    in
        { inputs0
            | x = minusPlus keys.left keys.right
            , y = minusPlus keys.down keys.up
            , button_X = keys.space
            , changeCamera = keys.kI
            , mx = minusPlus keys.kComma keys.kPeriod
        }


mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs ( mx, my ) inputs0 =
    { inputs0 | mx = toFloat mx / 500, my = toFloat my / 500 }


clearStationaryInputs : Model.Inputs -> Model.Inputs
clearStationaryInputs inputs0 =
    { inputs0 | mx = 0, my = 0 }


gamepadToInputs : Gamepad.Gamepad -> Model.Inputs -> Model.Inputs
gamepadToInputs gamepad0 inputs0 =
    let
        gamepad =
            GamepadInputs.toStandardGamepad gamepad0

        { x, y, mx, my, cx, cy } =
            GamepadInputs.gamepadToArrows gamepad

        bs =
            GamepadInputs.gamepadToButtons gamepad

        risingEdge old new =
            new && (not old)
    in
        { inputs0
            | reset = bs.bStart
            , changeVR = risingEdge inputs0.changeVR bs.bB
            , changeCamera = risingEdge inputs0.changeCamera bs.bRightBumper
            , x = x
            , y = y
            , mx = mx
            , my = my
            , cx = cx
            , cy = cy
            , button_X = risingEdge inputs0.button_X bs.bX
        }


updateGamepads : List Gamepad.Gamepad -> Model worldModel -> Model worldModel
updateGamepads gps0 model =
    let
        ( gps, is ) =
            GamepadInputs.persistentGamepads model.gamepadIds gps0
    in
        case gps of
            [] ->
                model

            [ Just gp ] ->
                { model
                    | numPlayers = 1
                    , inputs = gamepadToInputs gp model.inputs
                    , gamepadIds = is
                }

            (Just gp) :: Nothing :: _ ->
                { model
                    | numPlayers = 1
                    , inputs = gamepadToInputs gp model.inputs
                    , gamepadIds = is
                }

            Nothing :: (Just gp2) :: _ ->
                { model
                    | numPlayers = 2
                    , inputs2 = gamepadToInputs gp2 model.inputs2
                    , gamepadIds = is
                }

            (Just gp) :: (Just gp2) :: _ ->
                { model
                    | numPlayers = 2
                    , inputs = gamepadToInputs gp model.inputs
                    , inputs2 = gamepadToInputs gp2 model.inputs2
                    , gamepadIds = is
                }

            _ ->
                model


aboveGround : Model.EyeLevel -> Vec3 -> Vec3
aboveGround eyeLevel pos =
    let
        p =
            toRecord pos

        e =
            eyeLevel pos
    in
        if p.y < e then
            vec3 p.x e p.z
        else
            pos

shoot : Ground -> Model.Inputs -> Shot -> Framing -> Camera -> Camera
shoot ground inputs shot framing camera =
    let cameraInput =
            { x = inputs.cx
            , y = inputs.cy
            , dt = inputs.dt
            }
    in
        shot.shoot ground cameraInput framing.target camera

updatePlayer : Ground -> Model.Inputs -> String -> Maybe Shot -> Maybe Framing -> Model.Player -> Model.Player
updatePlayer terrain inputs label mshot framing player0 =
    if inputs.reset then
        Model.defaultPlayer
    else
        let
            eyeLevel pos =
                Model.eyeLevel + terrain.elevation pos

            shot =
                Maybe.withDefault tracking mshot

            relabel player =
                { player | rideLabel = label }

            mapCamera f player =
                { player | camera = f player.camera }

            shootFraming player =
                case framing of
                    Just framing_ ->
                        mapCamera (shoot terrain inputs shot framing_) player

                    Nothing ->
                        player

            smoothCamera player =
                let cameraPos =
                    -- (V3.add (V3.scale 0.9 player.camera.position) (V3.scale 0.1 player0.camera.position))
                    player.camera.position
                    |> aboveGround eyeLevel

                    -- TODO: slerp between old and new camera orientations
                    -- (V3.add (V3.scale 0.1 newCameraUp) (V3.scale 0.9 player.cameraUp))
                in
                    mapCamera (reposition cameraPos) player

                    -- { player
                    --     | camera = { c | position = cameraPos }
                   --  }

{-
            moveCamera player =
                let setCamera c = { player | camera = c }
                in
                if player.cameraInside then
                    let
                        inside =
                            add player.motion.position
                                (Orientation.rotateBodyV player.motion.orientation (vec3 0 0 3))

                        -- wedge
                        -- Inside Jeep driver's seat
                        -- `add` Qn.vrotate player.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        setCamera
                            { position = inside -- aboveGround eyeLevel inside
                            , orientation = player.camera.orientation
                            }
                else
                    let
                        newCameraPos = Follow.follow terrain player.motion

                        cameraPos =
                            (V3.add (V3.scale 0.5 newCameraPos) (V3.scale 0.5 player.camera.position)) -- smooth

                        newCameraOrientation =
                            player.motion.orientation

                        cameraOrientation = newCameraOrientation
                            -- TODO: slerp between old and new camera orientations
                            -- (V3.add (V3.scale 0.1 newCameraUp) (V3.scale 0.9 player.cameraUp))

                    in
                        setCamera
                            { position = terrain.bounds cameraPos
                            , orientation = cameraOrientation
                            }
-}
        in
            player0
                |> relabel
                |> shootFraming
                |> smoothCamera

nextShot : Shot -> Shot
nextShot shot =
    if shot.label == pov.label then
        tracking
    else if shot.label == tracking.label then
        dolly
    else if shot.label == dolly.label then
        pov
    else
        tracking

selectCamera : Ground -> (Bag.Key -> Bool) -> Bag.Key -> Model.Inputs -> Model.Player -> Model.Player
selectCamera ground hasFraming keyLimit inputs player =
    let
        nextKey key =
            (key + 1) % keyLimit

        findCameraHelp origKey key =
            if hasFraming key then
                key
            else
                let
                    next =
                        nextKey key
                in
                    if next == origKey then
                        origKey
                    else
                        findCameraHelp origKey next

        findCamera key =
            findCameraHelp key key

        key =
            findCamera (Maybe.withDefault 0 player.rideKey)

        newKey =
            if inputs.button_X then
                Just (findCamera (nextKey key))
            else
                Just key

        ensureShot =
            Maybe.withDefault tracking player.shot

        (newShot, newCamera) =
            if inputs.changeCamera then
                let shot = nextShot ensureShot
                in (Just shot, shot.init ground player.camera)
            else if inputs.button_X then
                (Just ensureShot, ensureShot.init ground player.camera)
            else
                (Just ensureShot, player.camera)

        newVR =
            if inputs.changeVR then
                not player.cameraVR
            else
                player.cameraVR

    in
        { player | rideKey = newKey
                 , shot = newShot
                 , cameraVR = newVR
        }
