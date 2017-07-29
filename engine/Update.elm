module Update exposing (update)

import Html exposing (Html)
import List.Extra as List
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Time exposing (Time)
import App exposing (Focus)
import Body exposing (reposition)
import Camera.Types exposing (..)
import Camera.POV exposing (pov)
import Camera.DollyArc exposing (dollyZoom, dolly, zoom, arc)
import Camera.Tracking exposing (tracking)
import Camera
import App.Control as Control
import Control exposing (WorldMsg, Route(..))
import Dispatch exposing (..)
import Gamepad
import GamepadInputs
import KeyboardInput
import Ground exposing (Ground)
import Methods exposing (Methods)
import Model exposing (Model, Msg, GlobalMsg(..), WorldKey(..), AppKey(..), PartyKey(..), PlayerKey(..))
import Orientation exposing (fromVec3)
import Ports


{-| Take a Msg and a Model and return an updated Model
-}
update :
    Methods worldFlags worldModel worldMsg
    -> Model.Msg navMsg (WorldMsg worldMsg)
    -> Model worldModel (WorldMsg worldMsg)
    -> ( Model worldModel (WorldMsg worldMsg), Cmd (Msg navMsg (WorldMsg worldMsg)) )
update world msg model =
    case msg of
        Model.WorldMessage worldMsg ->
            let
                ( multiverse, worldCmdMsg ) =
                    world.update worldMsg model.multiverse

                response x =
                    case x of
                        GlobalEffect e ->
                            Model.WorldEffect e

                        m ->
                            Model.WorldMessage m
            in
                ( { model | multiverse = multiverse }, Cmd.map response worldCmdMsg )

        Model.WorldEffect (PlayerUpdate oldPartyKey newPartyKey) ->
            let
                p1 =
                    model.player1

                p2 =
                    model.player2
            in
                if model.player1.partyKey == Just oldPartyKey then
                    ( { model | player1 = { p1 | partyKey = Just newPartyKey } }, Cmd.none )
                else if model.player2.partyKey == Just oldPartyKey then
                    ( { model | player2 = { p2 | partyKey = Just newPartyKey } }, Cmd.none )
                else
                    ( model, Cmd.none )

        Model.NavigatorMessage navMsg ->
            ( model, Cmd.none )
{-
            let
                ( navigator, navCmdMsg ) =
                    nav.update navMsg model.navigator

                response x =
                    case x of
                        GlobalEffect e ->
                            Model.NavigatorEffect e

                        m ->
                            Model.NavigatorMessage m
            in
                ( { model | navigator = navigator }, Cmd.map response navCmdMsg )
-}

        Model.NavigatorEffect (Model.ProvideInputs inputs) ->
            ( model, Cmd.none )

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

{-
        Model.MouseMove movement ->
            ( { model | inputs = mouseToInputs movement model.inputs }, Cmd.none )
-}

        Model.GamepadUpdate gps0 ->
            updateGamepads gps0 model

{-
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
-}

        Model.JoinWorld worldKey playerKey ->
            let
                ( partyKey, multiverse, cmdMsg ) =
                    world.join worldKey model.multiverse

                p1 =
                    model.player1

                p2 =
                    model.player2

                ( player1, player2 ) =
                    case playerKey of
                        PlayerKey 0 ->
                            ( { p1 | partyKey = partyKey }, p2 )

                        PlayerKey 1 ->
                            ( p1, { p2 | partyKey = partyKey } )

                        _ ->
                            ( p1, p2 )

                newModel =
                    { model
                        | player1 = player1
                        , player2 = player2
                        , multiverse = multiverse
                    }
            in
                ( newModel, Cmd.map Model.WorldMessage cmdMsg )

        Model.LeaveWorld (WorldKey worldKey playerKey) ->
            let
                leave mKey =
                    case mKey of
                        Nothing ->
                            model.multiverse

                        Just partyKey ->
                            world.leave partyKey model.multiverse

                p1 =
                    model.player1

                p2 =
                    model.player2

                ( multiverse, player1, player2 ) =
                    case playerKey of
                        PlayerKey 0 ->
                            ( leave p1.partyKey
                            , { p1 | partyKey = Nothing }
                            , p2
                            )

                        PlayerKey 1 ->
                            ( leave p2.partyKey
                            , p1
                            , { p2 | partyKey = Nothing }
                            )

                        _ ->
                            ( model.multiverse, p1, p2 )

                newModel =
                    { model
                        | player1 = player1
                        , player2 = player2
                        , multiverse = multiverse
                    }
            in
                ( newModel, Cmd.none )

        Model.Animate dt ->
            let
                toUnit (WorldKey n _) =
                    WorldKey n ()

                worldKeys =
                    List.filterMap identity [ model.player1.partyKey, model.player2.partyKey ]
                        |> List.map toUnit
                        |> List.uniqueBy (\(WorldKey n ()) -> n)

                -- updates : List (model -> (model, msg))
                updates =
                    List.map (animate world dt) worldKeys

                ( newModel, sequenceMsg ) =
                    sequenceUpdates updates model

                gamepadUpdateMsg =
                    Gamepad.gamepads Model.GamepadUpdate
            in
                ( newModel, Cmd.batch [ gamepadUpdateMsg, sequenceMsg ] )


sequenceUpdates : List (model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
sequenceUpdates =
    let
        s msgs updates model =
            case updates of
                [] ->
                    ( model, Cmd.batch msgs )

                f :: fs ->
                    let
                        ( newModel, newMsg ) =
                            f model
                    in
                        s (newMsg :: msgs) fs newModel
    in
        s []


animate :
    Methods worldFlags worldModel worldMsg
    -> Time
    -> WorldKey ()
    -> Model worldModel (WorldMsg worldMsg)
    -> ( Model worldModel (WorldMsg worldMsg), Cmd (Msg navMsg (WorldMsg worldMsg)) )
animate world dt0 worldKey model0 =
    case world.ground worldKey model0.multiverse of
        Nothing ->
            ( model0, Cmd.none )

        Just terrain ->
            let
                dt =
                    if model0.paused then
                        0
                    else
                        dt0

                -- Animate
                multiverseA =
                    world.animate worldKey terrain dt model0.multiverse

                {-
                   -- Focus
                   ( multiverseF, multiverseFMsg, focPos ) =
                       let
                           key =
                               player1.focusKey
                       in
                           case world.focus key model.multiverse of
                               Just focus ->
                                   let
                                       dp =
                                           inputsToMove inputs1 player1

                                       ( multiverseF, multiverseFMsg ) =
                                           world.update (Forward (ToApp key) (Control.Move dp)) multiverse2
                                   in
                                       ( multiverseF, multiverseFMsg, Just focus.position )

                               _ ->
                                   ( multiverse2, Cmd.none, Nothing )
                -}
                ( clearedInputs1, player1, multiverse1, multiverse1Msg ) =
                    animatePlayer world worldKey terrain dt0 model0.inputs model0.player1 multiverseA

                ( clearedInputs2, player2, multiverse2, multiverse2Msg ) =
                    animatePlayer world worldKey terrain dt0 model0.inputs2 model0.player2 multiverse1

                newModel =
                    { model0
                        | globalTime = model0.globalTime + dt
                        , player1 = player1
                        , player2 = player2
                        , inputs = clearedInputs1
                        , inputs2 = clearedInputs2
                        , multiverse = multiverse2
                    }
            in
                ( newModel
                , Cmd.map Model.WorldMessage (Cmd.batch [ multiverse1Msg, multiverse2Msg ])
                )


animatePlayer :
    Methods worldFlags worldModel worldMsg
    -> WorldKey ()
    -> Ground
    -> Time
    -> Model.Inputs
    -> Model.Player (WorldMsg worldMsg)
    -> worldModel
    -> ( Model.Inputs, Model.Player (WorldMsg worldMsg), worldModel, Cmd (WorldMsg worldMsg) )
animatePlayer world (WorldKey worldKey ()) terrain dt0 inputs0 player0 model =
    let
        inputs =
            timeToInputs dt0 inputs0

        justIfThisWorld (WorldKey wk p) =
            if wk == worldKey then
                Just (WorldKey wk p)
            else
                Nothing

        player =
            selectCamera terrain inputs player0
    in
        case Maybe.andThen justIfThisWorld player.partyKey of
            Just worldPartyKey ->
                let
                    ( rideModel, rideMsg ) =
                        if inputs.button_X then
                            world.changeRide worldPartyKey model
                        else
                            ( model, Cmd.none )

                    ( fwdModel, fwdMsg ) =
                        world.update
                            (Forward (ToParty worldPartyKey) (Control.Drive terrain inputs))
                            rideModel

                    label =
                        world.partyLabel worldPartyKey fwdModel

                    overlay =
                        world.overlay worldPartyKey fwdModel

                    framing =
                        world.framing worldPartyKey fwdModel

                    newPlayer =
                        updatePlayer terrain inputs dt0 label overlay player.shot framing player
                in
                    ( clearStationaryInputs inputs
                    , newPlayer
                    , fwdModel
                    , Cmd.batch [ rideMsg, fwdMsg ]
                    )

            Nothing ->
                ( inputs0
                , player0
                , model
                , Cmd.none
                )


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


updateGamepads :
    List Gamepad.Gamepad
    -> Model worldModel worldMsg
    -> ( Model worldModel worldMsg, Cmd (Msg navMsg worldMsg) )
updateGamepads gps0 model =
    let
        ( gps, is ) =
            GamepadInputs.persistentGamepads model.gamepadIds gps0

        joinIfNew =
            if model.numPlayers == 1 then
                Model.playerJoin (PlayerKey 1)
            else
                Cmd.none
    in
        case gps of
            [] ->
                ( model, Cmd.none )

            [ Just gp ] ->
                ( { model
                    | numPlayers = 1
                    , inputs = gamepadToInputs gp model.inputs
                    , gamepadIds = is
                  }
                , Cmd.none
                )

            (Just gp) :: Nothing :: _ ->
                ( { model
                    | numPlayers = 1
                    , inputs = gamepadToInputs gp model.inputs
                    , gamepadIds = is
                  }
                , Cmd.none
                )

            Nothing :: (Just gp2) :: _ ->
                ( { model
                    | numPlayers = 2
                    , inputs2 = gamepadToInputs gp2 model.inputs2
                    , gamepadIds = is
                  }
                , joinIfNew
                )

            (Just gp) :: (Just gp2) :: _ ->
                ( { model
                    | numPlayers = 2
                    , inputs = gamepadToInputs gp model.inputs
                    , inputs2 = gamepadToInputs gp2 model.inputs2
                    , gamepadIds = is
                  }
                , joinIfNew
                )

            _ ->
                ( model, Cmd.none )


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
    let
        cameraInput =
            { x = inputs.cx
            , y = inputs.cy
            , dt = dt
            }
    in
        shot.shoot ground cameraInput framing camera


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
                { player
                    | rideLabel = label
                    , overlayContent = overlayContent
                }

            shootFraming player =
                case framing of
                    Just framing_ ->
                        let
                            keepAboveGround camera =
                                reposition (aboveGround eyeLevel camera.position) camera

                            coeffs =
                                [ 1.0 ]

                            -- coeffs = [ 0.9, 0.09, 0.01 ]
                            -- coeffs = [ 0.6, 0.2, 0.1, 0.07, 0.03 ]
                            -- coeffs = [ 0.35, 0.23, 0.16, 0.11, 0.07, 0.04, 0.02, 0.01, 0.008, 0.002 ]
                            rawCamera =
                                shoot terrain inputs dt shot framing_ player.rawCamera
                                    |> keepAboveGround

                            newRecentRawCameras =
                                List.take (List.length coeffs) (rawCamera :: player.recentRawCameras)

                            newCamera =
                                -- Camera.interpolate 0.6 camera rawCamera
                                Camera.smooth coeffs newRecentRawCameras
                        in
                            { player
                                | camera = newCamera
                                , rawCamera = rawCamera
                                , recentRawCameras = newRecentRawCameras
                            }

                    Nothing ->
                        player

            {-
               mapCamera f player =
                   { player | camera = f player.camera }

               smoothCamera player =
                   let
                       cameraPos =
                           -- (V3.add (V3.scale 0.9 player.camera.position) (V3.scale 0.1 player0.camera.position))
                           player.camera.position
                               |> aboveGround eyeLevel

                       -- TODO: slerp between old and new camera orientations
                       -- (V3.add (V3.scale 0.1 newCameraUp) (V3.scale 0.9 player.cameraUp))

                   in
                       mapCamera (reposition cameraPos) player
            -}
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


prevShot : Shot -> Shot
prevShot shot =
    if shot.label == pov.label then
        zoom
    else if shot.label == tracking.label then
        pov
    else if shot.label == arc.label then
        tracking
    else if shot.label == dolly.label then
        arc
    else if shot.label == dollyZoom.label then
        dolly
    else if shot.label == zoom.label then
        dollyZoom
    else
        tracking


nextShot : Shot -> Shot
nextShot shot =
    if shot.label == pov.label then
        tracking
    else if shot.label == tracking.label then
        arc
    else if shot.label == arc.label then
        dolly
    else if shot.label == dolly.label then
        dollyZoom
    else if shot.label == dollyZoom.label then
        zoom
    else if shot.label == zoom.label then
        pov
    else
        tracking


selectCamera : Ground -> Model.Inputs -> Model.Player msg -> Model.Player msg
selectCamera ground inputs player =
    let
        ensureShot =
            Maybe.withDefault tracking player.shot

        ( newShot, newCamera ) =
            if inputs.prevCamera then
                let
                    shot =
                        prevShot ensureShot
                in
                    ( Just shot, shot.init ground player.camera )
            else if inputs.nextCamera then
                let
                    shot =
                        nextShot ensureShot
                in
                    ( Just shot, shot.init ground player.camera )
            else if inputs.button_X then
                ( Just ensureShot, ensureShot.init ground player.camera )
            else
                ( Just ensureShot, player.camera )

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
        { player
            | shot = newShot
            , cameraVR = newVR
            , overlayVisible = newOverlayVisible
        }
