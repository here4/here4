module Update exposing (update)

import Html exposing (Html)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Maybe.Extra exposing (isJust)
import Time exposing (Time)

import App exposing (Focus)
import Bag
import Body exposing (reposition)
import Camera exposing (..)
import Camera.POV exposing (pov)
import Camera.DollyArc exposing (dollyZoom, dolly, arc)
import Camera.Tracking exposing (tracking)
import Camera.Util as Camera
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Gamepad
import GamepadInputs
import KeyboardInput
import Ground exposing (Ground)
import Model exposing (Model, Msg)
import Orientation exposing (fromVec3)
import Ports


{-| Take a Msg and a Model and return an updated Model
-}
update :
    (WorldMsg worldMsg -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) ))
    -> (Maybe Bag.Key -> worldModel -> String)
    -> (Maybe Bag.Key -> worldModel -> Html (WorldMsg worldMsg))
    -> (worldModel -> Int)
    -> (worldModel -> Maybe Ground)
    -> (Ground -> Time -> worldModel -> worldModel)
    -> (Maybe Bag.Key -> worldModel -> Maybe Framing)
    -> (Bag.Key -> worldModel -> Maybe Focus)
    -> Model.Msg (WorldMsg worldMsg)
    -> Model worldModel (WorldMsg worldMsg)
    -> ( Model worldModel (WorldMsg worldMsg), Cmd (Msg (WorldMsg worldMsg)) )
update worldUpdate worldLabel worldOverlay worldKeyLimit worldTerrain worldAnimate worldFraming worldFocus msg model =
    case msg of
        Model.WorldMessage worldMsg ->
            let
                ( worldModel, worldCmdMsg ) =
                    worldUpdate worldMsg model.worldModel
            in
                ( { model | worldModel = worldModel }, Cmd.map Model.WorldMessage worldCmdMsg )

        Model.KeyChange keyfunc ->
            let
                risingEdge old new =
                    new && (not old)

                keys =
                    keyfunc model.keys

                pausePressed =
                    risingEdge model.keys.kP keys.kP

                paused =
                    if pausePressed then
                        not model.paused
                    else
                        model.paused
            in
                ( { model
                    | keys = keys
                    , paused = paused
                    , inputs = KeyboardInput.keysToInputs keys model.inputs
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

        Model.Animate dt0 ->
            let
                ( model_, newCmdMsg ) =
                    case worldTerrain model.worldModel of
                        Nothing ->
                            ( model, Cmd.none )

                        Just terrain ->
                            let
                                dt =
                                    if model.paused then
                                        0
                                    else
                                        dt0

                                inputs1 =
                                    timeToInputs dt model.inputs

                                inputs2 =
                                    timeToInputs dt model.inputs2

                                keyLimit =
                                    worldKeyLimit model.worldModel

                                -- Animate
                                wm =
                                    worldAnimate terrain dt model.worldModel

                                -- Change ride?
                                hasFraming key =
                                    isJust (worldFraming (Just key) wm)

                                player1 =
                                    selectCamera terrain hasFraming keyLimit inputs1 model.player1

                                player2 =
                                    selectCamera terrain hasFraming keyLimit inputs2 model.player2

                                label1 =
                                    worldLabel (player1.rideKey) wm

                                overlay1 =
                                    worldOverlay (player1.rideKey) wm

                                label2 =
                                    worldLabel (player2.rideKey) wm

                                overlay2 =
                                    worldOverlay (player2.rideKey) wm

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
                                        , player1 = updatePlayer terrain inputs1 dt0 label1 overlay1 player1.shot framing1 player1
                                        , player2 = updatePlayer terrain inputs2 dt0 label2 overlay2 player2.shot framing2 player2
                                        , inputs = clearStationaryInputs inputs1
                                        , worldModel = wmF
                                    }

                                gamepadUpdateMsg = Gamepad.gamepads Model.GamepadUpdate
                                wMsg = Cmd.map Model.WorldMessage wmFMsg
                            in
                                ( newModel, Cmd.batch [ gamepadUpdateMsg, wMsg ] )
            in
                ( model_ , newCmdMsg)


inputsToMove : Model.Inputs -> Model.Player msg -> Vec3
inputsToMove inputs player =
    let
        dp =
            vec3 -inputs.cx 0 inputs.cy
    in
        Orientation.rotateBodyV player.camera.orientation dp


timeToInputs : Time -> Model.Inputs -> Model.Inputs
timeToInputs dt inputs0 =
    { inputs0 | dt = dt }


mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs ( mx, my ) inputs =
    { inputs | mx = 0.5 * inputs.dt * toFloat mx, my = -0.5 * inputs.dt * toFloat my }


clearStationaryInputs : Model.Inputs -> Model.Inputs
clearStationaryInputs inputs0 =
    { inputs0 | mx = 0, my = 0 }


gamepadToInputs : Gamepad.Gamepad -> Model.Inputs -> Model.Inputs
gamepadToInputs gamepad inputs0 =
    let
        { x, y, mx, my, cx, cy, rightTrigger, leftTrigger } =
            GamepadInputs.gamepadToArrows gamepad

        bs =
            GamepadInputs.gamepadToButtons gamepad

        risingEdge old new =
            new && (not old)
    in
        { inputs0
            | prevCamera = risingEdge inputs0.prevCamera bs.bLeftBumper
            , nextCamera = risingEdge inputs0.nextCamera bs.bRightBumper
            , toggleOverlay = risingEdge inputs0.toggleOverlay bs.bGuide
            , prevOverlay = risingEdge inputs0.prevOverlay bs.bBack
            , nextOverlay = risingEdge inputs0.nextOverlay bs.bStart
            , x = x
            , y = y
            , mx = mx
            , my = my
            , cx = cx
            , cy = cy
            , button_X = risingEdge inputs0.button_X bs.bX
            , rightTrigger = rightTrigger
            , leftTrigger = leftTrigger
            --, changeVR = risingEdge inputs0.changeVR bs.bB
        }


updateGamepads : List Gamepad.Gamepad -> Model worldModel worldMsg -> Model worldModel worldMsg
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

shoot : Ground -> Model.Inputs -> Float -> Shot -> Framing -> Camera -> Camera
shoot ground inputs dt shot framing camera =
    let cameraInput =
            { x = inputs.cx
            , y = inputs.cy
            , dt = dt
            }
    in
        shot.shoot ground cameraInput framing.target camera

updatePlayer : Ground -> Model.Inputs -> Float -> String -> Html msg -> Maybe Shot -> Maybe Framing -> Model.Player msg -> Model.Player msg
updatePlayer terrain inputs dt label overlayContent mshot framing player0 =
    if inputs.reset then
        Model.defaultPlayer
    else
        let
            eyeLevel pos =
                Model.eyeLevel + terrain.elevation pos

            shot =
                Maybe.withDefault tracking mshot

            relabel player =
                { player | rideLabel = label
                         , overlayContent = overlayContent
                }

            mapCamera f player =
                { player | camera = f player.camera }

            shootFraming player =
                case framing of
                    Just framing_ ->
                        mapCamera (shoot terrain inputs dt shot framing_) player

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

prevShot : Shot -> Shot
prevShot shot =
    if shot.label == pov.label then
        arc
    else if shot.label == tracking.label then
        pov
    else if shot.label == dollyZoom.label then
        tracking
    else if shot.label == dolly.label then
        dollyZoom
    else if shot.label == arc.label then
        dolly
    else
        tracking

nextShot : Shot -> Shot
nextShot shot =
    if shot.label == pov.label then
        tracking
    else if shot.label == tracking.label then
        dollyZoom
    else if shot.label == dollyZoom.label then
        dolly
    else if shot.label == dolly.label then
        arc
    else if shot.label == arc.label then
        pov
    else
        tracking

selectCamera : Ground -> (Bag.Key -> Bool) -> Bag.Key -> Model.Inputs -> Model.Player msg -> Model.Player msg
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
            if inputs.prevCamera then
                let shot = prevShot ensureShot
                in (Just shot, shot.init ground player.camera)
            else if inputs.nextCamera then
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

        newOverlayVisible =
            if inputs.toggleOverlay then
                not player.overlayVisible
            else
                player.overlayVisible

    in
        { player | rideKey = newKey
                 , shot = newShot
                 , cameraVR = newVR
                 , overlayVisible = newOverlayVisible
        }
