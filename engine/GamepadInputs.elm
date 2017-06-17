module GamepadInputs exposing (GamepadButtons, persistentGamepads, gamepadToArrows, gamepadToButtons)

import String exposing (contains)
import Gamepad exposing (..)


----------------------------------------------------------------------
-- Gamepad


gamepadToArrows : Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadToArrows gamepad =
    case gamepad of
        StandardGamepad g ->
            let
                sgn x =
                    if x < 0 then
                        -1
                    else
                        1

                -- Apply a deadzone of size l around an axis centered at 0
                u l x =
                    if abs x < l then
                        0.0
                    else
                        sgn x * (abs x - l) / (1.0 - l)

                deadzone =
                    u 0.2

                axs =
                    { x = deadzone g.rightStick.x
                    , y = deadzone (-1.0 * g.rightStick.y)
                    , mx = deadzone g.leftStick.x * 0.1
                    , my = deadzone (-1.0 * g.leftStick.y) * 0.1
                    , cx = 0
                    , cy = 0
                    }

                -- Interpret brake, accelerator as y input
                ( btns_y, cx, cy ) =
                    ( g.rightTrigger.value - g.leftTrigger.value
                    , g.dPadRight.value - g.dPadLeft.value
                    , g.dPadUp.value - g.dPadDown.value
                    )
            in
                { axs | y = btns_y + axs.y, cx = cx, cy = cy }

        RawGamepad g ->
            { x = 0, y = 0, mx = 0, my = 0, cx = 0, cy = 0 }


gamepadsToArrows : List Gamepad.Gamepad -> List { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadsToArrows =
    List.map gamepadToArrows


persistentGamepads : List String -> List Gamepad.Gamepad -> ( List (Maybe Gamepad.Gamepad), List String )
persistentGamepads is0 gs0 =
    let
        gamepadId g =
            case g of
                StandardGamepad g_ ->
                    g_.id

                RawGamepad g_ ->
                    g_.id

        extract acc i gs0 =
            case gs0 of
                [] ->
                    ( Nothing, List.reverse acc ++ gs0 )

                g :: gs ->
                    if gamepadId g == i then
                        ( Just g, List.reverse acc ++ gs )
                    else
                        extract (g :: acc) i gs

        reorder is0 gs0 =
            case is0 of
                [] ->
                    List.map Just (List.sortBy gamepadId gs0)

                i :: is ->
                    let
                        ( gm, gs ) =
                            extract [] i gs0
                    in
                        gm :: reorder is gs

        catMaybes =
            List.filterMap Basics.identity

        remap : List String -> List (Maybe Gamepad.Gamepad) -> List String
        remap ids0 gs0 =
            case ( ids0, gs0 ) of
                ( [], _ ) ->
                    catMaybes (List.map (Maybe.map gamepadId) gs0)

                ( is, [] ) ->
                    is

                ( i :: is, g :: gs ) ->
                    Maybe.withDefault i (Maybe.map gamepadId g) :: remap is gs

        gs =
            reorder is0 gs0
    in
        ( gs, remap is0 gs )


type alias GamepadButtons =
    { bA : Bool
    , bB : Bool
    , bX : Bool
    , bY : Bool
    , bLeftBumper : Bool
    , bRightBumper : Bool
    , bBack : Bool
    , bStart : Bool
    }


gamepadButtonsNone : GamepadButtons
gamepadButtonsNone =
    { bA = False
    , bB = False
    , bX = False
    , bY = False
    , bLeftBumper = False
    , bRightBumper = False
    , bBack = False
    , bStart = False
    }


gamepadToButtons : Gamepad.Gamepad -> GamepadButtons
gamepadToButtons gamepad =
    case gamepad of
        StandardGamepad g ->
            { bA = g.buttonA.pressed
            , bB = g.buttonB.pressed
            , bX = g.buttonX.pressed
            , bY = g.buttonY.pressed
            , bLeftBumper = g.leftBumper.pressed
            , bRightBumper = g.rightBumper.pressed
            , bBack = g.buttonBack.pressed
            , bStart = g.buttonStart.pressed
            }

        _ ->
            gamepadButtonsNone


gamepadsToButtons : List Gamepad.Gamepad -> List GamepadButtons
gamepadsToButtons =
    List.map gamepadToButtons
